// Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#define class class_
#define private private_
#include <R_ext/Connections.h>
#undef private
#undef class

#if !defined(R_CONNECTIONS_VERSION) || R_CONNECTIONS_VERSION != 1
#error "socketR requires R_CONNECTIONS_VERSION == 1"
#endif

#include <arpa/inet.h>
#include <cerrno>
#include <cmath>
#include <cstddef>
#include <cstring>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdexcept>
#include <string>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <vector>

namespace {

[[noreturn]] void stopf(const char* message) {
  throw std::runtime_error(message);
}

template <typename Arg, typename... Args>
[[noreturn]] void stopf(const char* format, Arg arg, Args... args) {
  int size = std::snprintf(nullptr, 0, format, arg, args...);
  if (size < 0) throw std::runtime_error("socketR native error");
  std::vector<char> buffer(static_cast<size_t>(size) + 1);
  std::snprintf(buffer.data(), buffer.size(), format, arg, args...);
  throw std::runtime_error(buffer.data());
}

#define CALL_BEGIN try {
#define CALL_END                                                            \
  }                                                                         \
  catch (const std::exception& ex) {                                         \
    Rf_error("%s", ex.what());                                               \
  }                                                                         \
  catch (...) {                                                             \
    Rf_error("unknown socketR native error");                                \
  }                                                                         \
  return R_NilValue;

class SocketHandle {
public:
  int fd;
  int domain;
  int type;
  int protocol;

  SocketHandle(int fd_, int domain_, int type_, int protocol_)
      : fd(fd_), domain(domain_), type(type_), protocol(protocol_) {}

  ~SocketHandle() {
    if (fd >= 0) {
      ::close(fd);
      fd = -1;
    }
  }

  SocketHandle(const SocketHandle&) = delete;
  SocketHandle& operator=(const SocketHandle&) = delete;
};

struct ConnectionState {
  SEXP socket;
  bool close_socket;
};

class FdGuard {
public:
  explicit FdGuard(int fd_) : fd(fd_) {}
  ~FdGuard() {
    if (fd >= 0) ::close(fd);
  }
  int release() {
    int out = fd;
    fd = -1;
    return out;
  }
  FdGuard(const FdGuard&) = delete;
  FdGuard& operator=(const FdGuard&) = delete;

private:
  int fd;
};

std::string errno_name(int err) {
  switch (err) {
#ifdef EACCES
  case EACCES: return "EACCES";
#endif
#ifdef EADDRINUSE
  case EADDRINUSE: return "EADDRINUSE";
#endif
#ifdef EADDRNOTAVAIL
  case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
#endif
#ifdef EAFNOSUPPORT
  case EAFNOSUPPORT: return "EAFNOSUPPORT";
#endif
#ifdef EAGAIN
  case EAGAIN: return "EAGAIN";
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || EWOULDBLOCK != EAGAIN)
  case EWOULDBLOCK: return "EWOULDBLOCK";
#endif
#ifdef EBADF
  case EBADF: return "EBADF";
#endif
#ifdef ECONNREFUSED
  case ECONNREFUSED: return "ECONNREFUSED";
#endif
#ifdef ECONNRESET
  case ECONNRESET: return "ECONNRESET";
#endif
#ifdef EINPROGRESS
  case EINPROGRESS: return "EINPROGRESS";
#endif
#ifdef EINTR
  case EINTR: return "EINTR";
#endif
#ifdef EINVAL
  case EINVAL: return "EINVAL";
#endif
#ifdef EISCONN
  case EISCONN: return "EISCONN";
#endif
#ifdef ENETUNREACH
  case ENETUNREACH: return "ENETUNREACH";
#endif
#ifdef ENOENT
  case ENOENT: return "ENOENT";
#endif
#ifdef ENOTCONN
  case ENOTCONN: return "ENOTCONN";
#endif
#ifdef EPIPE
  case EPIPE: return "EPIPE";
#endif
#ifdef ETIMEDOUT
  case ETIMEDOUT: return "ETIMEDOUT";
#endif
  default: return "errno";
  }
}

void raise_errno(const std::string& what, int err = errno) {
  stopf("%s failed: %s (%s, errno=%d)", what.c_str(),
        errno_name(err).c_str(), std::strerror(err), err);
}

bool is_would_block(int err) {
#ifdef EWOULDBLOCK
  return err == EAGAIN || err == EWOULDBLOCK;
#else
  return err == EAGAIN;
#endif
}

std::string to_lower(std::string value) {
  for (char& c : value) {
    if (c >= 'A' && c <= 'Z') c = static_cast<char>(c - 'A' + 'a');
  }
  return value;
}

std::string as_string_scalar(SEXP value, const char* arg) {
  if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1 ||
      STRING_ELT(value, 0) == NA_STRING) {
    stopf("`%s` must be a non-missing character scalar", arg);
  }
  return Rf_translateCharUTF8(STRING_ELT(value, 0));
}

int as_int_scalar(SEXP value, const char* arg) {
  if (XLENGTH(value) != 1) {
    stopf("`%s` must be a non-missing integer scalar", arg);
  }
  if (TYPEOF(value) == INTSXP) {
    int out = INTEGER(value)[0];
    if (out != NA_INTEGER) return out;
  } else if (TYPEOF(value) == REALSXP) {
    double out = REAL(value)[0];
    if (R_FINITE(out)) return static_cast<int>(out);
  }
  stopf("`%s` must be a non-missing integer scalar", arg);
}

bool as_bool_scalar(SEXP value, const char* arg) {
  if (TYPEOF(value) != LGLSXP || XLENGTH(value) != 1 ||
      LOGICAL(value)[0] == NA_LOGICAL) {
    stopf("`%s` must be TRUE or FALSE", arg);
  }
  return LOGICAL(value)[0] == TRUE;
}

SocketHandle* get_handle(SEXP xp, bool require_open = true) {
  if (TYPEOF(xp) != EXTPTRSXP) {
    stopf("`socket` must be a socketR external pointer");
  }
  SocketHandle* handle = static_cast<SocketHandle*>(R_ExternalPtrAddr(xp));
  if (handle == nullptr) {
    stopf("socket handle is not valid");
  }
  if (require_open && handle->fd < 0) {
    stopf("socket is closed");
  }
  return handle;
}

SocketHandle* connection_handle(Rconnection con) {
  if (con == nullptr || con->ex_ptr == nullptr) return nullptr;
  ConnectionState* state = static_cast<ConnectionState*>(con->ex_ptr);
  if (TYPEOF(state->socket) != EXTPTRSXP) return nullptr;
  SocketHandle* handle =
      static_cast<SocketHandle*>(R_ExternalPtrAddr(state->socket));
  return handle != nullptr && handle->fd >= 0 ? handle : nullptr;
}

Rboolean socket_connection_open(Rconnection con) {
  if (connection_handle(con) == nullptr) return FALSE;
  con->isopen = TRUE;
  return TRUE;
}

void socket_connection_close(Rconnection con) {
  ConnectionState* state = static_cast<ConnectionState*>(con->ex_ptr);
  if (state != nullptr && state->close_socket) {
    SocketHandle* handle = TYPEOF(state->socket) == EXTPTRSXP
                               ? static_cast<SocketHandle*>(
                                     R_ExternalPtrAddr(state->socket))
                               : nullptr;
    if (handle != nullptr && handle->fd >= 0) {
      ::close(handle->fd);
      handle->fd = -1;
    }
    con->isopen = FALSE;
  } else {
    // Preserve the default adapter-only close behavior.
    con->isopen = TRUE;
  }
}

void socket_connection_destroy(Rconnection con) {
  if (con->ex_ptr != nullptr) {
    ConnectionState* state = static_cast<ConnectionState*>(con->ex_ptr);
    R_ReleaseObject(state->socket);
    delete state;
    con->ex_ptr = nullptr;
  }
}

int socket_connection_fgetc(Rconnection con) {
  unsigned char value = 0;
  SocketHandle* handle = connection_handle(con);
  if (handle == nullptr) return EOF;
  ssize_t n;
  do {
    n = ::recv(handle->fd, &value, 1, 0);
  } while (n < 0 && errno == EINTR);
  if (n <= 0) return EOF;
  return value;
}

size_t socket_connection_read(void* buffer, size_t size, size_t nitems,
                              Rconnection con) {
  if (size == 0 || nitems == 0) return 0;
  SocketHandle* handle = connection_handle(con);
  if (handle == nullptr) return 0;
  size_t total = size * nitems;
  size_t offset = 0;
  while (offset < total) {
    ssize_t n = ::recv(handle->fd,
                       static_cast<unsigned char*>(buffer) + offset,
                       total - offset, 0);
    if (n > 0) {
      offset += static_cast<size_t>(n);
      continue;
    }
    if (n < 0 && errno == EINTR) continue;
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;
    break;
  }
  if (offset == 0) return 0;
  con->isopen = TRUE;
  return offset / size;
}

size_t socket_connection_write(const void* buffer, size_t size, size_t nitems,
                               Rconnection con) {
  if (size == 0 || nitems == 0) return 0;
  SocketHandle* handle = connection_handle(con);
  if (handle == nullptr) return 0;
  size_t total = size * nitems;
  size_t offset = 0;
  while (offset < total) {
    ssize_t n = ::send(handle->fd,
                       static_cast<const unsigned char*>(buffer) + offset,
                       total - offset, 0);
    if (n > 0) {
      offset += static_cast<size_t>(n);
      continue;
    }
    if (n < 0 && errno == EINTR) continue;
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;
    break;
  }
  if (offset == 0) return 0;
  con->isopen = TRUE;
  return offset / size;
}

int socket_connection_fflush(Rconnection con) {
  // Keep the adapter open after libraries flush their output sink.
  con->isopen = TRUE;
  return 0;
}

double socket_connection_seek(Rconnection, double, int, int) {
  return -1;
}

int parse_domain(SEXP domain) {
  std::string d = to_lower(as_string_scalar(domain, "domain"));
  if (d == "inet" || d == "ipv4" || d == "af_inet") return AF_INET;
  if (d == "inet6" || d == "ipv6" || d == "af_inet6") return AF_INET6;
  if (d == "unix" || d == "local" || d == "af_unix") return AF_UNIX;
  stopf("unsupported socket domain: %s", d.c_str());
}

int parse_type(SEXP type) {
  std::string t = to_lower(as_string_scalar(type, "type"));
  if (t == "stream" || t == "tcp" || t == "sock_stream") return SOCK_STREAM;
  if (t == "dgram" || t == "datagram" || t == "udp" || t == "sock_dgram") return SOCK_DGRAM;
  stopf("unsupported socket type: %s", t.c_str());
}

std::string domain_name(int domain) {
  switch (domain) {
  case AF_INET: return "inet";
  case AF_INET6: return "inet6";
  case AF_UNIX: return "unix";
  default: return "unknown";
  }
}

std::string type_name(int type) {
  int base_type = type;
#ifdef SOCK_NONBLOCK
  base_type &= ~SOCK_NONBLOCK;
#endif
#ifdef SOCK_CLOEXEC
  base_type &= ~SOCK_CLOEXEC;
#endif
  switch (base_type) {
  case SOCK_STREAM: return "stream";
  case SOCK_DGRAM: return "dgram";
  default: return "unknown";
  }
}

void set_cloexec_fd(int fd, bool cloexec) {
  int flags = ::fcntl(fd, F_GETFD, 0);
  if (flags < 0) raise_errno("fcntl(F_GETFD)");
  if (cloexec) flags |= FD_CLOEXEC;
  else flags &= ~FD_CLOEXEC;
  if (::fcntl(fd, F_SETFD, flags) < 0) raise_errno("fcntl(F_SETFD)");
}

void set_blocking_fd(int fd, bool blocking) {
  int flags = ::fcntl(fd, F_GETFL, 0);
  if (flags < 0) raise_errno("fcntl(F_GETFL)");
  if (blocking) flags &= ~O_NONBLOCK;
  else flags |= O_NONBLOCK;
  if (::fcntl(fd, F_SETFL, flags) < 0) raise_errno("fcntl(F_SETFL)");
}

bool fd_is_blocking(int fd) {
  int flags = ::fcntl(fd, F_GETFL, 0);
  if (flags < 0) raise_errno("fcntl(F_GETFL)");
  return (flags & O_NONBLOCK) == 0;
}

void socket_finalizer(SEXP xp) {
  SocketHandle* handle = static_cast<SocketHandle*>(R_ExternalPtrAddr(xp));
  delete handle;
  R_ClearExternalPtr(xp);
}

SEXP make_xptr(int fd, int domain, int type, int protocol) {
  SocketHandle* handle = new SocketHandle(fd, domain, type, protocol);
  SEXP ptr = PROTECT(R_MakeExternalPtr(handle, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ptr, socket_finalizer, TRUE);
  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("socketr_socket"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("externalptr"));
  Rf_setAttrib(ptr, R_ClassSymbol, classes);
  UNPROTECT(2);
  return ptr;
}

const sockaddr* as_sockaddr(const sockaddr_storage& storage) {
  return reinterpret_cast<const sockaddr*>(&storage);
}

sockaddr* as_sockaddr(sockaddr_storage& storage) {
  return reinterpret_cast<sockaddr*>(&storage);
}

void make_sockaddr(int domain, const std::string& address, int port,
                   sockaddr_storage& storage, socklen_t& len) {
  std::memset(&storage, 0, sizeof(storage));

  if (domain == AF_INET || domain == AF_INET6) {
    if (port < 0 || port > 65535) stopf("`port` must be between 0 and 65535");

    char service[6];
    std::snprintf(service, sizeof(service), "%d", port);
    addrinfo hints;
    std::memset(&hints, 0, sizeof(hints));
    hints.ai_family = domain;
    hints.ai_socktype = 0;
    hints.ai_flags = AI_NUMERICSERV;
    if (address.empty()) hints.ai_flags |= AI_PASSIVE;

    addrinfo* results = nullptr;
    int rc = ::getaddrinfo(address.empty() ? nullptr : address.c_str(),
                           service, &hints, &results);
    if (rc != 0) {
      stopf("could not resolve socket address '%s': %s",
            address.c_str(), gai_strerror(rc));
    }
    if (results == nullptr || results->ai_addrlen > sizeof(storage)) {
      if (results != nullptr) ::freeaddrinfo(results);
      stopf("resolved socket address is invalid");
    }
    std::memcpy(&storage, results->ai_addr, results->ai_addrlen);
    len = static_cast<socklen_t>(results->ai_addrlen);
    ::freeaddrinfo(results);
    return;
  }

  if (domain == AF_UNIX) {
    sockaddr_un addr;
    std::memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    if (address.empty()) stopf("Unix-domain socket path must not be empty");
    if (address.size() >= sizeof(addr.sun_path)) {
      stopf("Unix-domain socket path is too long for sockaddr_un");
    }
    std::memcpy(addr.sun_path, address.c_str(), address.size() + 1);
    std::memcpy(&storage, &addr, sizeof(addr));
    len = static_cast<socklen_t>(offsetof(sockaddr_un, sun_path) + address.size() + 1);
    return;
  }

  stopf("unsupported socket domain");
}

SEXP make_named_list(const std::vector<const char*>& names) {
  R_xlen_t n = static_cast<R_xlen_t>(names.size());
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, n));
  for (R_xlen_t i = 0; i < n; ++i) {
    SET_STRING_ELT(out_names, i, Rf_mkChar(names[static_cast<size_t>(i)]));
  }
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(2);
  return out;
}

SEXP sockaddr_to_list(const sockaddr_storage& storage, socklen_t len) {
  const sockaddr* sa = as_sockaddr(storage);
  char buf[INET6_ADDRSTRLEN];

  if (sa->sa_family == AF_INET) {
    const sockaddr_in* addr = reinterpret_cast<const sockaddr_in*>(sa);
    if (::inet_ntop(AF_INET, &addr->sin_addr, buf, sizeof(buf)) == nullptr) {
      raise_errno("inet_ntop");
    }
    SEXP out = PROTECT(make_named_list({"family", "address", "port"}));
    SET_VECTOR_ELT(out, 0, Rf_mkString("inet"));
    SET_VECTOR_ELT(out, 1, Rf_mkString(buf));
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(ntohs(addr->sin_port)));
    UNPROTECT(1);
    return out;
  }

  if (sa->sa_family == AF_INET6) {
    const sockaddr_in6* addr = reinterpret_cast<const sockaddr_in6*>(sa);
    if (::inet_ntop(AF_INET6, &addr->sin6_addr, buf, sizeof(buf)) == nullptr) {
      raise_errno("inet_ntop");
    }
    SEXP out = PROTECT(make_named_list(
        {"family", "address", "port", "flowinfo", "scope_id"}));
    SET_VECTOR_ELT(out, 0, Rf_mkString("inet6"));
    SET_VECTOR_ELT(out, 1, Rf_mkString(buf));
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(ntohs(addr->sin6_port)));
    SET_VECTOR_ELT(out, 3, Rf_ScalarReal(addr->sin6_flowinfo));
    SET_VECTOR_ELT(out, 4, Rf_ScalarReal(addr->sin6_scope_id));
    UNPROTECT(1);
    return out;
  }

  if (sa->sa_family == AF_UNIX) {
    const sockaddr_un* addr = reinterpret_cast<const sockaddr_un*>(sa);
    std::string path;
    if (len > offsetof(sockaddr_un, sun_path)) {
      size_t path_len = len - offsetof(sockaddr_un, sun_path);
      if (path_len > sizeof(addr->sun_path)) path_len = sizeof(addr->sun_path);
      if (path_len > 0 && addr->sun_path[path_len - 1] == '\0') --path_len;
      path.assign(addr->sun_path, path_len);
    }
    SEXP out = PROTECT(make_named_list({"family", "path"}));
    SET_VECTOR_ELT(out, 0, Rf_mkString("unix"));
    SET_VECTOR_ELT(out, 1, Rf_mkString(path.c_str()));
    UNPROTECT(1);
    return out;
  }

  SEXP out = PROTECT(make_named_list({"family"}));
  SET_VECTOR_ELT(out, 0, Rf_mkString("unknown"));
  UNPROTECT(1);
  return out;
}

int parse_level(SEXP level) {
  if (TYPEOF(level) == INTSXP || TYPEOF(level) == REALSXP) return as_int_scalar(level, "level");
  std::string s = to_lower(as_string_scalar(level, "level"));
  if (s == "socket" || s == "sol_socket") return SOL_SOCKET;
  if (s == "tcp" || s == "ipproto_tcp") return IPPROTO_TCP;
  if (s == "ip" || s == "ipproto_ip") return IPPROTO_IP;
  if (s == "ipv6" || s == "ipproto_ipv6") return IPPROTO_IPV6;
  stopf("unsupported socket option level: %s", s.c_str());
}

int parse_option(SEXP option, int level) {
  if (TYPEOF(option) == INTSXP || TYPEOF(option) == REALSXP) return as_int_scalar(option, "option");
  std::string s = to_lower(as_string_scalar(option, "option"));

  if (level == SOL_SOCKET) {
    if (s == "reuseaddr" || s == "reuse_address") return SO_REUSEADDR;
#ifdef SO_REUSEPORT
    if (s == "reuseport" || s == "reuse_port") return SO_REUSEPORT;
#endif
    if (s == "keepalive" || s == "keep_alive") return SO_KEEPALIVE;
    if (s == "broadcast") return SO_BROADCAST;
    if (s == "rcvbuf" || s == "receive_buffer" || s == "receive_buffer_size") return SO_RCVBUF;
    if (s == "sndbuf" || s == "send_buffer" || s == "send_buffer_size") return SO_SNDBUF;
    if (s == "rcvtimeo" || s == "receive_timeout") return SO_RCVTIMEO;
    if (s == "sndtimeo" || s == "send_timeout") return SO_SNDTIMEO;
    if (s == "linger") return SO_LINGER;
    if (s == "error") return SO_ERROR;
  }

  if (level == IPPROTO_TCP) {
#ifdef TCP_NODELAY
    if (s == "nodelay" || s == "no_delay" || s == "tcp_nodelay") return TCP_NODELAY;
#endif
#ifdef TCP_QUICKACK
    if (s == "quickack" || s == "quick_ack" || s == "tcp_quickack") return TCP_QUICKACK;
#endif
  }

  if (level == IPPROTO_IPV6) {
#ifdef IPV6_V6ONLY
    if (s == "v6only" || s == "ipv6_v6only") return IPV6_V6ONLY;
#endif
  }

  if (level == IPPROTO_IP) {
#ifdef IP_TTL
    if (s == "ttl") return IP_TTL;
#endif
#ifdef IP_MULTICAST_LOOP
    if (s == "multicast_loop") return IP_MULTICAST_LOOP;
#endif
  }

  stopf("unsupported socket option for this level: %s", s.c_str());
}

SEXP make_raw_result(const unsigned char* data, ssize_t n) {
  if (n <= 0) return Rf_allocVector(RAWSXP, 0);
  SEXP out = Rf_allocVector(RAWSXP, static_cast<R_xlen_t>(n));
  std::memcpy(RAW(out), data, static_cast<size_t>(n));
  return out;
}

} // namespace

extern "C" SEXP socketr_socket_create(SEXP domain_s, SEXP type_s,
                                      SEXP protocol_s, SEXP nonblocking_s,
                                      SEXP cloexec_s) {
CALL_BEGIN
  int domain = parse_domain(domain_s);
  int type = parse_type(type_s);
  int protocol = as_int_scalar(protocol_s, "protocol");
  bool nonblocking = as_bool_scalar(nonblocking_s, "nonblocking");
  bool cloexec = as_bool_scalar(cloexec_s, "cloexec");

  int fd = ::socket(domain, type, protocol);
  if (fd < 0) raise_errno("socket");

  FdGuard guard(fd);
  if (cloexec) set_cloexec_fd(fd, true);
  if (nonblocking) set_blocking_fd(fd, false);

  return make_xptr(guard.release(), domain, type, protocol);
CALL_END
}

extern "C" SEXP socketr_socket_connection(SEXP socket_s, SEXP close_socket_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  bool close_socket = as_bool_scalar(close_socket_s, "close_socket");
  if (handle->fd < 0) stopf("socket is closed");

  Rconnection connection = nullptr;
  SEXP result = PROTECT(R_new_custom_connection(
      "socketR socket", "r+b", "socketr_connection", &connection));
  connection->open = socket_connection_open;
  connection->close = socket_connection_close;
  connection->destroy = socket_connection_destroy;
  connection->fgetc = socket_connection_fgetc;
  connection->fgetc_internal = socket_connection_fgetc;
  connection->read = socket_connection_read;
  connection->write = socket_connection_write;
  connection->fflush = socket_connection_fflush;
  connection->seek = socket_connection_seek;
  connection->canread = TRUE;
  connection->canwrite = TRUE;
  connection->canseek = FALSE;
  connection->text = FALSE;
  connection->blocking = fd_is_blocking(handle->fd) ? TRUE : FALSE;
  connection->isopen = TRUE;
  ConnectionState* state = new ConnectionState{socket_s, close_socket};
  connection->ex_ptr = static_cast<void*>(state);
  R_PreserveObject(socket_s);
  UNPROTECT(1);
  return result;
CALL_END
}

extern "C" SEXP socketr_connection_read(SEXP connection_s, SEXP n_s) {
CALL_BEGIN
  Rconnection connection = R_GetConnection(connection_s);
  int n = as_int_scalar(n_s, "n");
  if (n < 0) stopf("`n` must be non-negative");
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, n));
  size_t read = R_ReadConnection(connection, RAW(result), static_cast<size_t>(n));
  if (read == static_cast<size_t>(n)) {
    UNPROTECT(1);
    return result;
  }
  SEXP trimmed = PROTECT(Rf_allocVector(RAWSXP, static_cast<R_xlen_t>(read)));
  if (read > 0) std::memcpy(RAW(trimmed), RAW(result), read);
  UNPROTECT(2);
  return trimmed;
CALL_END
}

extern "C" SEXP socketr_connection_write(SEXP connection_s, SEXP data_s) {
CALL_BEGIN
  Rconnection connection = R_GetConnection(connection_s);
  if (TYPEOF(data_s) != RAWSXP) stopf("`data` must be a raw vector");
  size_t written = R_WriteConnection(
      connection, RAW(data_s), static_cast<size_t>(XLENGTH(data_s)));
  return Rf_ScalarInteger(static_cast<int>(written));
CALL_END
}

extern "C" SEXP socketr_resolve(SEXP address_s, SEXP domain_s, SEXP port_s) {
CALL_BEGIN
  std::string address = as_string_scalar(address_s, "address");
  int domain = parse_domain(domain_s);
  if (domain == AF_UNIX) {
    stopf("hostname resolution is not supported for Unix-domain sockets");
  }
  int port = as_int_scalar(port_s, "port");
  if (port < 0 || port > 65535) stopf("`port` must be between 0 and 65535");

  char service[6];
  std::snprintf(service, sizeof(service), "%d", port);
  addrinfo hints;
  std::memset(&hints, 0, sizeof(hints));
  hints.ai_family = domain;
  hints.ai_socktype = 0;
  hints.ai_flags = AI_NUMERICSERV;
  addrinfo* results = nullptr;
  int rc = ::getaddrinfo(address.c_str(), service, &hints, &results);
  if (rc != 0) {
    stopf("could not resolve socket address '%s': %s",
          address.c_str(), gai_strerror(rc));
  }

  size_t resolved_count = 0;
  for (addrinfo* item = results; item != nullptr; item = item->ai_next) {
    if (item->ai_addrlen <= sizeof(sockaddr_storage)) ++resolved_count;
  }
  if (resolved_count == 0) {
    ::freeaddrinfo(results);
    stopf("resolver returned no usable socket addresses");
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, resolved_count));
  R_xlen_t index = 0;
  for (addrinfo* item = results; item != nullptr; item = item->ai_next) {
    if (item->ai_addrlen > sizeof(sockaddr_storage)) continue;
    sockaddr_storage storage;
    std::memset(&storage, 0, sizeof(storage));
    std::memcpy(&storage, item->ai_addr, item->ai_addrlen);
    SET_VECTOR_ELT(
        out, index++,
        sockaddr_to_list(storage, static_cast<socklen_t>(item->ai_addrlen)));
  }
  ::freeaddrinfo(results);
  UNPROTECT(1);
  return out;
CALL_END
}

extern "C" SEXP socketr_socket_close(SEXP socket_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s, false);
  if (handle->fd < 0) return Rf_ScalarLogical(TRUE);
  int fd = handle->fd;
  handle->fd = -1;
  if (::close(fd) < 0) raise_errno("close");
  return Rf_ScalarLogical(TRUE);
CALL_END
}

extern "C" SEXP socketr_socket_is_open(SEXP socket_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s, false);
  return Rf_ScalarLogical(handle->fd >= 0);
CALL_END
}

extern "C" SEXP socketr_socket_fd(SEXP socket_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s, false);
  if (handle->fd < 0) return Rf_ScalarInteger(NA_INTEGER);
  return Rf_ScalarInteger(handle->fd);
CALL_END
}

extern "C" SEXP socketr_socket_info(SEXP socket_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s, false);
  bool open = handle->fd >= 0;
  SEXP out = PROTECT(make_named_list(
      {"fd", "open", "domain", "type", "protocol", "blocking"}));
  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(open ? handle->fd : NA_INTEGER));
  SET_VECTOR_ELT(out, 1, Rf_ScalarLogical(open));
  SET_VECTOR_ELT(out, 2, Rf_mkString(domain_name(handle->domain).c_str()));
  SET_VECTOR_ELT(out, 3, Rf_mkString(type_name(handle->type).c_str()));
  SET_VECTOR_ELT(out, 4, Rf_ScalarInteger(handle->protocol));
  SET_VECTOR_ELT(out, 5, Rf_ScalarLogical(
      open ? (fd_is_blocking(handle->fd) ? TRUE : FALSE) : NA_LOGICAL));
  UNPROTECT(1);
  return out;
CALL_END
}

extern "C" SEXP socketr_socket_set_blocking(SEXP socket_s, SEXP blocking_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  bool blocking = as_bool_scalar(blocking_s, "blocking");
  set_blocking_fd(handle->fd, blocking);
  return socket_s;
CALL_END
}

extern "C" SEXP socketr_socket_bind(SEXP socket_s, SEXP address_s, SEXP port_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  std::string address = as_string_scalar(address_s, "address");
  int port = (handle->domain == AF_UNIX) ? 0 : as_int_scalar(port_s, "port");

  sockaddr_storage addr;
  socklen_t len = 0;
  make_sockaddr(handle->domain, address, port, addr, len);
  if (::bind(handle->fd, as_sockaddr(addr), len) < 0) raise_errno("bind");
  return socket_s;
CALL_END
}

extern "C" SEXP socketr_socket_listen(SEXP socket_s, SEXP backlog_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int backlog = as_int_scalar(backlog_s, "backlog");
  if (backlog < 0) stopf("`backlog` must be non-negative");
  if (::listen(handle->fd, backlog) < 0) raise_errno("listen");
  return socket_s;
CALL_END
}

extern "C" SEXP socketr_socket_accept(SEXP socket_s, SEXP nonblocking_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  if (TYPEOF(nonblocking_s) != LGLSXP || XLENGTH(nonblocking_s) != 1) {
    stopf("`nonblocking` must be TRUE, FALSE, or NA");
  }
  int nb = LOGICAL(nonblocking_s)[0];
  bool accepted_nonblocking =
      nb == NA_LOGICAL ? !fd_is_blocking(handle->fd) : nb == TRUE;

  sockaddr_storage peer;
  socklen_t peer_len = sizeof(peer);
  int fd;
  do {
    fd = ::accept(handle->fd, as_sockaddr(peer), &peer_len);
  } while (fd < 0 && errno == EINTR);

  if (fd < 0) {
    int err = errno;
    if (is_would_block(err)) return R_NilValue;
    raise_errno("accept", err);
  }

  FdGuard guard(fd);
  set_cloexec_fd(fd, true);
  set_blocking_fd(fd, !accepted_nonblocking);

  return make_xptr(guard.release(), handle->domain, handle->type, handle->protocol);
CALL_END
}

extern "C" SEXP socketr_socket_connect(SEXP socket_s, SEXP address_s,
                                       SEXP port_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  std::string address = as_string_scalar(address_s, "address");
  int port = (handle->domain == AF_UNIX) ? 0 : as_int_scalar(port_s, "port");

  sockaddr_storage addr;
  socklen_t len = 0;
  make_sockaddr(handle->domain, address, port, addr, len);
  int rc;
  do {
    rc = ::connect(handle->fd, as_sockaddr(addr), len);
  } while (rc < 0 && errno == EINTR);

  if (rc < 0) {
    int err = errno;
    if (err == EINPROGRESS || err == EALREADY) return Rf_ScalarLogical(FALSE);
    if (err == EISCONN) return Rf_ScalarLogical(TRUE);
    raise_errno("connect", err);
  }
  return Rf_ScalarLogical(TRUE);
CALL_END
}

extern "C" SEXP socketr_socket_send(SEXP socket_s, SEXP data_s, SEXP flags_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  if (TYPEOF(data_s) != RAWSXP) stopf("`data` must be a raw vector");
  int flags = as_int_scalar(flags_s, "flags");
  ssize_t n;
  do {
    n = ::send(handle->fd, RAW(data_s),
               static_cast<size_t>(XLENGTH(data_s)), flags);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    int err = errno;
    if (is_would_block(err)) return Rf_ScalarInteger(NA_INTEGER);
    raise_errno("send", err);
  }
  return Rf_ScalarInteger(static_cast<int>(n));
CALL_END
}

extern "C" SEXP socketr_socket_recv(SEXP socket_s, SEXP nbytes_s, SEXP flags_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int nbytes = as_int_scalar(nbytes_s, "nbytes");
  int flags = as_int_scalar(flags_s, "flags");
  if (nbytes < 0) stopf("`nbytes` must be non-negative");
  if (nbytes == 0) return Rf_allocVector(RAWSXP, 0);

  std::vector<unsigned char> buffer(static_cast<size_t>(nbytes));
  ssize_t n;
  do {
    n = ::recv(handle->fd, buffer.data(), buffer.size(), flags);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    int err = errno;
    if (is_would_block(err)) return R_NilValue;
    raise_errno("recv", err);
  }
  return make_raw_result(buffer.data(), n);
CALL_END
}

extern "C" SEXP socketr_socket_sendto(SEXP socket_s, SEXP data_s,
                                      SEXP address_s, SEXP port_s,
                                      SEXP flags_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  if (TYPEOF(data_s) != RAWSXP) stopf("`data` must be a raw vector");
  std::string address = as_string_scalar(address_s, "address");
  int port = (handle->domain == AF_UNIX) ? 0 : as_int_scalar(port_s, "port");
  int flags = as_int_scalar(flags_s, "flags");

  sockaddr_storage addr;
  socklen_t len = 0;
  make_sockaddr(handle->domain, address, port, addr, len);
  ssize_t n;
  do {
    n = ::sendto(handle->fd, RAW(data_s),
                 static_cast<size_t>(XLENGTH(data_s)), flags,
                 as_sockaddr(addr), len);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    int err = errno;
    if (is_would_block(err)) return Rf_ScalarInteger(NA_INTEGER);
    raise_errno("sendto", err);
  }
  return Rf_ScalarInteger(static_cast<int>(n));
CALL_END
}

extern "C" SEXP socketr_socket_recvfrom(SEXP socket_s, SEXP nbytes_s,
                                        SEXP flags_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int nbytes = as_int_scalar(nbytes_s, "nbytes");
  int flags = as_int_scalar(flags_s, "flags");
  if (nbytes < 0) stopf("`nbytes` must be non-negative");

  std::vector<unsigned char> buffer(static_cast<size_t>(nbytes));
  sockaddr_storage peer;
  std::memset(&peer, 0, sizeof(peer));
  socklen_t peer_len = sizeof(peer);
  ssize_t n;
  do {
    n = ::recvfrom(handle->fd, buffer.data(), buffer.size(), flags,
                   as_sockaddr(peer), &peer_len);
  } while (n < 0 && errno == EINTR);
  if (n < 0) {
    int err = errno;
    if (is_would_block(err)) return R_NilValue;
    raise_errno("recvfrom", err);
  }
  SEXP out = PROTECT(make_named_list({"data", "address"}));
  SET_VECTOR_ELT(out, 0, make_raw_result(buffer.data(), n));
  SET_VECTOR_ELT(out, 1, sockaddr_to_list(peer, peer_len));
  UNPROTECT(1);
  return out;
CALL_END
}

extern "C" SEXP socketr_socket_shutdown(SEXP socket_s, SEXP how_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int how = as_int_scalar(how_s, "how");
  if (::shutdown(handle->fd, how) < 0) raise_errno("shutdown");
  return socket_s;
CALL_END
}

extern "C" SEXP socketr_socket_name(SEXP socket_s, SEXP peer_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  bool peer_name = as_bool_scalar(peer_s, "peer");
  sockaddr_storage addr;
  std::memset(&addr, 0, sizeof(addr));
  socklen_t len = sizeof(addr);
  int rc = peer_name ? ::getpeername(handle->fd, as_sockaddr(addr), &len)
                     : ::getsockname(handle->fd, as_sockaddr(addr), &len);
  if (rc < 0) raise_errno(peer_name ? "getpeername" : "getsockname");
  return sockaddr_to_list(addr, len);
CALL_END
}

extern "C" SEXP socketr_socket_poll(SEXP sockets_s, SEXP events_s,
                                    SEXP timeout_s) {
CALL_BEGIN
  if (TYPEOF(sockets_s) != VECSXP) stopf("`sockets` must be a list");
  if (TYPEOF(events_s) != INTSXP) stopf("`events` must be an integer vector");
  int timeout = as_int_scalar(timeout_s, "timeout_ms");
  if (timeout < -1) stopf("`timeout_ms` must be -1 or non-negative");
  if (XLENGTH(events_s) != XLENGTH(sockets_s)) {
    stopf("`events` length must match `sockets`");
  }

  R_xlen_t n = XLENGTH(sockets_s);
  std::vector<pollfd> fds(static_cast<size_t>(n));
  for (R_xlen_t i = 0; i < n; ++i) {
    SocketHandle* handle = get_handle(VECTOR_ELT(sockets_s, i));
    short requested = 0;
    if (INTEGER(events_s)[i] & 1) requested |= POLLIN;
    if (INTEGER(events_s)[i] & 2) requested |= POLLOUT;
    requested |= POLLERR | POLLHUP | POLLNVAL;
    fds[static_cast<size_t>(i)].fd = handle->fd;
    fds[static_cast<size_t>(i)].events = requested;
    fds[static_cast<size_t>(i)].revents = 0;
  }

  int rc;
  do {
    rc = ::poll(fds.data(), fds.size(), timeout);
  } while (rc < 0 && errno == EINTR);
  if (rc < 0) raise_errno("poll");

  SEXP out = PROTECT(make_named_list(
      {"ready_count", "readable", "writable", "error", "hangup", "invalid",
       "revents"}));
  SEXP readable = PROTECT(Rf_allocVector(LGLSXP, n));
  SEXP writable = PROTECT(Rf_allocVector(LGLSXP, n));
  SEXP error = PROTECT(Rf_allocVector(LGLSXP, n));
  SEXP hangup = PROTECT(Rf_allocVector(LGLSXP, n));
  SEXP invalid = PROTECT(Rf_allocVector(LGLSXP, n));
  SEXP revents = PROTECT(Rf_allocVector(INTSXP, n));
  for (R_xlen_t i = 0; i < n; ++i) {
    short rev = fds[static_cast<size_t>(i)].revents;
    LOGICAL(readable)[i] = (rev & POLLIN) ? TRUE : FALSE;
    LOGICAL(writable)[i] = (rev & POLLOUT) ? TRUE : FALSE;
    LOGICAL(error)[i] = (rev & POLLERR) ? TRUE : FALSE;
    LOGICAL(hangup)[i] = (rev & POLLHUP) ? TRUE : FALSE;
    LOGICAL(invalid)[i] = (rev & POLLNVAL) ? TRUE : FALSE;
    INTEGER(revents)[i] = rev;
  }

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(rc));
  SET_VECTOR_ELT(out, 1, readable);
  SET_VECTOR_ELT(out, 2, writable);
  SET_VECTOR_ELT(out, 3, error);
  SET_VECTOR_ELT(out, 4, hangup);
  SET_VECTOR_ELT(out, 5, invalid);
  SET_VECTOR_ELT(out, 6, revents);
  UNPROTECT(7);
  return out;
CALL_END
}

extern "C" SEXP socketr_socket_get_option(SEXP socket_s, SEXP level_s,
                                          SEXP option_s, SEXP type_s,
                                          SEXP size_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int level = parse_level(level_s);
  int option = parse_option(option_s, level);
  std::string type = to_lower(as_string_scalar(type_s, "type"));

  if (type == "int" || type == "integer" || type == "logical" || type == "bool") {
    int value = 0;
    socklen_t len = sizeof(value);
    if (::getsockopt(handle->fd, level, option, &value, &len) < 0) raise_errno("getsockopt");
    if (type == "logical" || type == "bool") {
      return Rf_ScalarLogical(value != 0);
    }
    return Rf_ScalarInteger(value);
  }

  if (type == "timeval" || type == "timeout") {
    timeval value;
    std::memset(&value, 0, sizeof(value));
    socklen_t len = sizeof(value);
    if (::getsockopt(handle->fd, level, option, &value, &len) < 0) raise_errno("getsockopt");
    double seconds = static_cast<double>(value.tv_sec) + static_cast<double>(value.tv_usec) / 1000000.0;
    return Rf_ScalarReal(seconds);
  }

  if (type == "linger") {
    linger value;
    std::memset(&value, 0, sizeof(value));
    socklen_t len = sizeof(value);
    if (::getsockopt(handle->fd, level, option, &value, &len) < 0) raise_errno("getsockopt");
    SEXP out = PROTECT(make_named_list({"on", "seconds"}));
    SET_VECTOR_ELT(out, 0, Rf_ScalarLogical(value.l_onoff != 0));
    SET_VECTOR_ELT(out, 1, Rf_ScalarInteger(value.l_linger));
    UNPROTECT(1);
    return out;
  }

  if (type == "raw") {
    int size = as_int_scalar(size_s, "size");
    if (size <= 0) stopf("`size` must be positive for raw socket options");
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, size));
    socklen_t len = static_cast<socklen_t>(size);
    if (::getsockopt(handle->fd, level, option, RAW(out), &len) < 0) raise_errno("getsockopt");
    SEXP trimmed = PROTECT(Rf_allocVector(RAWSXP, len));
    if (len > 0) std::memcpy(RAW(trimmed), RAW(out), len);
    UNPROTECT(2);
    return trimmed;
  }

  stopf("unsupported socket option type: %s", type.c_str());
CALL_END
}

extern "C" SEXP socketr_socket_set_option(SEXP socket_s, SEXP level_s,
                                          SEXP option_s, SEXP value_s,
                                          SEXP type_s) {
CALL_BEGIN
  SocketHandle* handle = get_handle(socket_s);
  int level = parse_level(level_s);
  int option = parse_option(option_s, level);
  std::string type = to_lower(as_string_scalar(type_s, "type"));

  if (type == "int" || type == "integer") {
    int value = as_int_scalar(value_s, "value");
    if (::setsockopt(handle->fd, level, option, &value, sizeof(value)) < 0) raise_errno("setsockopt");
    return socket_s;
  }

  if (type == "logical" || type == "bool") {
    int value = as_bool_scalar(value_s, "value") ? 1 : 0;
    if (::setsockopt(handle->fd, level, option, &value, sizeof(value)) < 0) raise_errno("setsockopt");
    return socket_s;
  }

  if (type == "timeval" || type == "timeout") {
    if ((TYPEOF(value_s) != REALSXP && TYPEOF(value_s) != INTSXP) ||
        XLENGTH(value_s) != 1) {
      stopf("`value` must be a non-negative numeric scalar for timeval options");
    }
    double x = TYPEOF(value_s) == REALSXP ? REAL(value_s)[0]
                                          : INTEGER(value_s)[0];
    if (!R_FINITE(x) || x < 0) {
      stopf("`value` must be a non-negative numeric scalar for timeval options");
    }
    double whole;
    double fractional = std::modf(x, &whole);
    timeval value;
    value.tv_sec = static_cast<time_t>(whole);
    value.tv_usec = static_cast<suseconds_t>(std::round(fractional * 1000000.0));
    if (value.tv_usec >= 1000000) {
      value.tv_sec += 1;
      value.tv_usec -= 1000000;
    }
    if (::setsockopt(handle->fd, level, option, &value, sizeof(value)) < 0) raise_errno("setsockopt");
    return socket_s;
  }

  if (type == "linger") {
    if (TYPEOF(value_s) != VECSXP) {
      stopf("linger `value` must be a list with `on` and `seconds`");
    }
    SEXP names = Rf_getAttrib(value_s, R_NamesSymbol);
    int on_index = -1;
    int seconds_index = -1;
    for (R_xlen_t i = 0; i < XLENGTH(value_s); ++i) {
      if (TYPEOF(names) != STRSXP || i >= XLENGTH(names)) break;
      const char* name = CHAR(STRING_ELT(names, i));
      if (std::strcmp(name, "on") == 0) on_index = static_cast<int>(i);
      if (std::strcmp(name, "seconds") == 0) {
        seconds_index = static_cast<int>(i);
      }
    }
    if (on_index < 0 || seconds_index < 0) {
      stopf("linger `value` must be a list with `on` and `seconds`");
    }
    linger value;
    value.l_onoff =
        as_bool_scalar(VECTOR_ELT(value_s, on_index), "value$on") ? 1 : 0;
    int seconds =
        as_int_scalar(VECTOR_ELT(value_s, seconds_index), "value$seconds");
    if (seconds < 0) stopf("`value$seconds` must be non-negative");
    value.l_linger = seconds;
    if (::setsockopt(handle->fd, level, option, &value, sizeof(value)) < 0) raise_errno("setsockopt");
    return socket_s;
  }

  if (type == "raw") {
    if (TYPEOF(value_s) != RAWSXP) stopf("`value` must be a raw vector");
    if (::setsockopt(handle->fd, level, option, RAW(value_s),
                     static_cast<socklen_t>(XLENGTH(value_s))) < 0) {
      raise_errno("setsockopt");
    }
    return socket_s;
  }

  stopf("unsupported socket option type: %s", type.c_str());
CALL_END
}

static const R_CallMethodDef CallEntries[] = {
  {"socketr_socket_create",      (DL_FUNC) &socketr_socket_create,      5},
  {"socketr_socket_connection",  (DL_FUNC) &socketr_socket_connection,  2},
  {"socketr_connection_read",    (DL_FUNC) &socketr_connection_read,    2},
  {"socketr_connection_write",   (DL_FUNC) &socketr_connection_write,   2},
  {"socketr_resolve",            (DL_FUNC) &socketr_resolve,            3},
  {"socketr_socket_close",       (DL_FUNC) &socketr_socket_close,       1},
  {"socketr_socket_is_open",     (DL_FUNC) &socketr_socket_is_open,     1},
  {"socketr_socket_fd",          (DL_FUNC) &socketr_socket_fd,          1},
  {"socketr_socket_info",        (DL_FUNC) &socketr_socket_info,        1},
  {"socketr_socket_set_blocking",(DL_FUNC) &socketr_socket_set_blocking,2},
  {"socketr_socket_bind",        (DL_FUNC) &socketr_socket_bind,        3},
  {"socketr_socket_listen",      (DL_FUNC) &socketr_socket_listen,      2},
  {"socketr_socket_accept",      (DL_FUNC) &socketr_socket_accept,      2},
  {"socketr_socket_connect",     (DL_FUNC) &socketr_socket_connect,     3},
  {"socketr_socket_send",        (DL_FUNC) &socketr_socket_send,        3},
  {"socketr_socket_recv",        (DL_FUNC) &socketr_socket_recv,        3},
  {"socketr_socket_sendto",      (DL_FUNC) &socketr_socket_sendto,      5},
  {"socketr_socket_recvfrom",    (DL_FUNC) &socketr_socket_recvfrom,    3},
  {"socketr_socket_shutdown",    (DL_FUNC) &socketr_socket_shutdown,    2},
  {"socketr_socket_name",        (DL_FUNC) &socketr_socket_name,        2},
  {"socketr_socket_poll",        (DL_FUNC) &socketr_socket_poll,        3},
  {"socketr_socket_get_option",  (DL_FUNC) &socketr_socket_get_option,  5},
  {"socketr_socket_set_option",  (DL_FUNC) &socketr_socket_set_option,  5},
  {NULL, NULL, 0}
};

extern "C" void R_init_socketR(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
