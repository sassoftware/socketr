# Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

as_socket_data <- function(data) {
  if (is.raw(data)) return(data)
  if (is.character(data) && length(data) == 1L && !is.na(data)) return(charToRaw(data))
  stop("`data` must be a raw vector or a non-missing character scalar", call. = FALSE)
}

check_socket <- function(socket) {
  if (!inherits(socket, "socketr_socket")) {
    stop("`socket` must be a socketR socket handle", call. = FALSE)
  }
  socket
}

socketr_call <- function(.NAME, ...) {
  if (.Platform$OS.type == "windows") {
    stop("socketR socket operations are not available on Windows", call. = FALSE)
  }
  .Call(deparse(substitute(.NAME)), ...)
}

socket_metadata <- function(socket, local = FALSE, peer = FALSE) {
  info <- socket_info(socket)
  attributes <- list(
    domain = info$domain,
    family = info$domain,
    type = info$type,
    protocol = info$protocol,
    fd = info$fd,
    blocking = info$blocking,
    open = info$open
  )
  if (local && isTRUE(info$open)) {
    endpoint <- socket_local_name(socket)
    attributes$address <- endpoint$address
    attributes$port <- endpoint$port
  }
  if (peer && isTRUE(info$open)) {
    endpoint <- socket_peer_name(socket)
    attributes$peer_address <- endpoint$address
    attributes$peer_port <- endpoint$port
  }
  attributes
}

socket_update_attributes <- function(socket, local = FALSE, peer = FALSE) {
  metadata <- socket_metadata(socket, local, peer)
  for (name in names(metadata)) attr(socket, name) <- metadata[[name]]
  socket
}

#' @export
`$.socketr_socket` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

socket_domain <- function(socket) socket_info(socket)$domain

parse_auto_endpoint <- function(address, port, allow_null = FALSE) {
  if (is.null(address) && allow_null) {
    if (is.null(port) || length(port) != 1L || is.na(port)) {
      stop("`port` must be a non-missing integer scalar", call. = FALSE)
    }
    port <- as.integer(port)
    if (port < 0L || port > 65535L) {
      stop("`port` must be between 0 and 65535", call. = FALSE)
    }
    return(list(address = NULL, port = port))
  }
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    stop("`address` must be a non-missing hostname or address scalar", call. = FALSE)
  }
  match <- regexec("^\\[([^]]+)\\](?::([0-9]+))?$", address, perl = TRUE)
  parts <- regmatches(address, match)[[1L]]
  if (length(parts)) {
    if (!is.null(port) && length(port) == 1L && !is.na(port) &&
        nzchar(parts[3L])) {
      stop("specify the port either in `address` or in `port`, not both",
           call. = FALSE)
    }
    address <- parts[2L]
    if (nzchar(parts[3L])) port <- as.integer(parts[3L])
  } else {
    host_port <- regexec("^([^:]+):([0-9]+)$", address, perl = TRUE)
    host_parts <- regmatches(address, host_port)[[1L]]
    if (length(host_parts)) {
      if (!is.null(port) && length(port) == 1L && !is.na(port)) {
        stop("specify the port either in `address` or in `port`, not both",
             call. = FALSE)
      }
      address <- host_parts[2L]
      port <- as.integer(host_parts[3L])
    }
  }
  if (is.null(port) || length(port) != 1L || is.na(port)) {
    stop("`port` must be a non-missing integer scalar", call. = FALSE)
  }
  port <- as.integer(port)
  if (port < 0L || port > 65535L) {
    stop("`port` must be between 0 and 65535", call. = FALSE)
  }
  list(address = address, port = port)
}

normalize_address_port <- function(socket, address, port, bind = FALSE) {
  domain <- socket_domain(socket)
  if (domain == "unix") {
    if (is.null(address) || !is.character(address) || length(address) != 1L || is.na(address)) {
      stop("Unix-domain sockets require a non-missing path in `address`", call. = FALSE)
    }
    return(list(address = address, port = 0L))
  }

  if (is.null(address)) {
    address <- if (bind && identical(domain, "inet6")) "::" else if (bind) "0.0.0.0" else if (identical(domain, "inet6")) "::1" else "127.0.0.1"
    if (is.null(port) || length(port) != 1L || is.na(port)) {
      stop("`port` must be a non-missing integer scalar", call. = FALSE)
    }
    port <- as.integer(port)
    if (port < 0L || port > 65535L) {
      stop("`port` must be between 0 and 65535", call. = FALSE)
    }
  } else {
    endpoint <- parse_auto_endpoint(address, port)
    address <- endpoint$address
    port <- endpoint$port
  }
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    stop("`address` must be a non-missing hostname or address scalar", call. = FALSE)
  }
  list(address = address, port = port)
}

shutdown_how <- function(how) {
  how <- match.arg(how, c("read", "write", "both"))
  switch(how, read = 0L, write = 1L, both = 2L)
}

events_to_mask <- function(events) {
  if (is.integer(events) || is.numeric(events)) return(as.integer(events))
  events <- match.arg(events, c("read", "write", "readwrite", "rw"), several.ok = TRUE)
  as.integer((any(events %in% c("read", "readwrite", "rw"))) + 2L * any(events %in% c("write", "readwrite", "rw")))
}

#' Create and configure POSIX sockets.
#'
#' Create a POSIX socket.
#' @rdname socket_setup
#' @name socket_setup
#' @param domain Address family: `"inet"`, `"inet6"`, or `"unix"`.
#' @param type Socket type: `"stream"` or `"dgram"`.
#' @param protocol Numeric protocol, usually zero.
#' @param nonblocking Whether the socket starts in nonblocking mode.
#' @param cloexec Whether to set close-on-exec.
#' @return A socketR external-pointer handle.
#' @examples
#' s <- socket_create()
#' socket_close(s)
#' @export
socket_create <- function(domain = c("inet", "inet6", "unix"), type = c("stream", "dgram"),
                          protocol = 0L, nonblocking = FALSE, cloexec = TRUE) {
  domain <- match.arg(domain)
  type <- match.arg(type)
  socket <- socketr_call(socketr_socket_create, domain, type, as.integer(protocol),
                         isTRUE(nonblocking), isTRUE(cloexec))
  socket_update_attributes(socket)
}

#' Resolve a hostname or address into socket endpoints.
#'
#' @param address A hostname or numeric IP address.
#' @param domain Address family, such as `"inet"` or `"inet6"`.
#' @param port Numeric service port between 0 and 65535.
#' @return A list of resolved endpoint metadata.
#' @export
socket_resolve <- function(address, domain = c("inet", "inet6"), port = 0L) {
  domain <- match.arg(domain)
  endpoint <- parse_auto_endpoint(address, port)
  socketr_call(socketr_resolve, endpoint$address, domain, endpoint$port)
}

#' Manage a socket handle lifecycle.
#'
#' @rdname socket_lifecycle
#' @name socket_lifecycle
#' @param socket A socketR socket handle.
#' @return Invisibly, `TRUE`.
#' @examples
#' s <- socket_create()
#' socket_close(s)
#' @export
socket_close <- function(socket) {
  socket <- check_socket(socket)
  socketr_call(socketr_socket_close, socket)
  invisible(socket_update_attributes(socket))
}

#' Close a socketR handle with the standard R connection API.
#'
#' @param con A socketR socket handle.
#' @param ... Ignored.
#' @export
close.socketr_socket <- function(con, ...) {
  socket_close(con)
}

#' Test whether a socket handle is open.
#' @rdname socket_lifecycle
#' @param socket A socketR socket handle.
#' @return A logical scalar.
#' @export
socket_is_open <- function(socket) {
  socketr_call(socketr_socket_is_open, check_socket(socket))[[1L]]
}

#' Return the native file descriptor, or NA for a closed socket.
#' @rdname socket_inspection
#' @param socket A socketR socket handle.
#' @return An integer descriptor or `NA_integer_`.
#' @export
socket_fd <- function(socket) {
  socketr_call(socketr_socket_fd, check_socket(socket))[[1L]]
}

#' Return socket metadata.
#' @rdname socket_inspection
#' @param socket A socketR socket handle.
#' @return A list containing socket metadata.
#' @export
socket_info <- function(socket) {
  socketr_call(socketr_socket_info, check_socket(socket))
}

#' Control socket blocking and readiness.
#'
#' @rdname socket_readiness
#' @name socket_readiness
#' @param socket A socketR socket handle.
#' @param blocking Whether operations should block.
#' @return Invisibly, the socket handle.
#' @examples
#' s <- socket_create()
#' socket_set_blocking(s, FALSE)
#' socket_close(s)
#' @export
socket_set_blocking <- function(socket, blocking = TRUE) {
  socket <- check_socket(socket)
  socketr_call(socketr_socket_set_blocking, socket, isTRUE(blocking))
  invisible(socket_update_attributes(socket))
}

#' Bind a socket to a local address.
#' @rdname socket_setup
#' @param socket A socketR socket handle.
#' @param address A hostname, IP address, or Unix-domain path.
#' @param port A port between 0 and 65535; `NULL` for Unix sockets.
#' @return Invisibly, the socket handle.
#' @export
socket_bind <- function(socket, address = NULL, port = NULL) {
  target <- normalize_address_port(check_socket(socket), address, port, bind = TRUE)
  socketr_call(socketr_socket_bind, socket, target$address, target$port)
  invisible(socket_update_attributes(socket, local = TRUE))
}

#' Listen for stream connections.
#' @rdname socket_setup
#' @param socket A stream socket handle.
#' @param backlog Maximum pending connection queue length.
#' @return Invisibly, the socket handle.
#' @export
socket_listen <- function(socket, backlog = 128L) {
  invisible(socketr_call(socketr_socket_listen, check_socket(socket), as.integer(backlog)))
}

#' Accept a pending connection. Returns NULL when a nonblocking listener would block.
#' @rdname socket_setup
#' @param socket A listening socket handle.
#' @param nonblocking Whether the accepted socket should be nonblocking; `NULL`
#'   inherits the listener mode.
#' @return A socket handle, or `NULL` when no connection is pending.
#' @export
socket_accept <- function(socket, nonblocking = NULL) {
  nb <- if (is.null(nonblocking)) NA else isTRUE(nonblocking)
  accepted <- socketr_call(socketr_socket_accept, check_socket(socket), nb)
  if (!is.null(accepted)) socket_update_attributes(accepted, local = TRUE, peer = TRUE)
  accepted
}

#' Connect a socket. Returns FALSE for a nonblocking connect in progress.
#' @rdname socket_setup
#' @param socket A socketR socket handle.
#' @param address A hostname, IP address, or Unix-domain path.
#' @param port A port between 0 and 65535; `NULL` for Unix sockets.
#' @return `TRUE` when connected, or `FALSE` when connection is in progress.
#' @export
socket_connect <- function(socket, address, port = NULL) {
  target <- normalize_address_port(check_socket(socket), address, port, bind = FALSE)
  connected <- socketr_call(socketr_socket_connect, socket, target$address, target$port)[[1L]]
  if (isTRUE(connected)) socket_update_attributes(socket, local = TRUE, peer = TRUE)
  connected
}

#' Connect using the first available IPv6 or IPv4 endpoint.
#'
#' Resolves `address` for both requested address families, tries endpoints in
#' `prefer` order, and creates a socket matching the endpoint that succeeds.
#' This is the address-aware alternative to an `"auto"` socket domain.
#' It supports IPv4 and IPv6 Internet sockets only; Unix-domain filesystem
#' paths are not resolved by this helper. Use `type = "dgram"` for UDP or the
#' default `type = "stream"` for TCP.
#'
#' @param address A hostname or numeric IP address.
#' @param port A port between 0 and 65535, or `NULL` when embedded in a
#' bracketed endpoint such as `"[::1]:12345"`.
#' @param type Socket type: `"stream"` or `"dgram"`.
#' @param protocol Numeric protocol, usually zero.
#' @param nonblocking Whether the socket should be nonblocking.
#' @param cloexec Whether to set close-on-exec.
#' @param prefer Address families to try, in order.
#' @return A connected socketR handle.
#' @examples
#' if (interactive()) {
#'   client <- socket_connect_auto("localhost", 80L)
#'   socket_close(client)
#' }
#' @export
socket_connect_auto <- function(address, port = NULL, type = c("stream", "dgram"),
                                protocol = 0L, nonblocking = FALSE,
                                cloexec = TRUE, prefer = c("inet6", "inet")) {
  endpoint <- parse_auto_endpoint(address, port)
  address <- endpoint$address
  port <- endpoint$port
  type <- match.arg(type)
  prefer <- match.arg(prefer, c("inet", "inet6"), several.ok = TRUE)

  endpoints <- unlist(lapply(prefer, function(domain) {
    tryCatch(socket_resolve(address, domain, port), error = function(error) NULL)
  }), recursive = FALSE)
  if (!length(endpoints)) {
    stop(sprintf("could not resolve '%s' as an IPv4 or IPv6 endpoint", address),
         call. = FALSE)
  }

  last_error <- NULL
  for (endpoint in endpoints) {
    socket <- socket_create(endpoint$family, type, protocol, nonblocking, cloexec)
    connected <- tryCatch(
      socket_connect(socket, endpoint$address, endpoint$port),
      error = identity
    )
    if (!inherits(connected, "error") && (isTRUE(connected) || isTRUE(nonblocking))) {
      return(socket)
    }
    last_error <- if (inherits(connected, "error")) connected else
      simpleError("connection did not complete")
    socket_close(socket)
  }

  stop(sprintf("could not connect to '%s:%d': %s", address, port,
               conditionMessage(last_error)), call. = FALSE)
}

#' Listen on the first available IPv6 or IPv4 address family.
#'
#' Creates a stream listener, tries bind candidates in `prefer` order, and
#' returns the first listener that binds and listens successfully. When
#' `address` is `NULL`, the family wildcard (`"::"` or `"0.0.0.0"`) is used.
#' This helper supports IPv4 and IPv6 stream sockets only. Unix-domain sockets
#' require an explicit filesystem path with [socket_create()] and
#' [socket_bind()].
#'
#' @param address A local hostname or IP address, or `NULL` for a wildcard.
#' @param port A port between 0 and 65535, or `NULL` when embedded in a
#' bracketed endpoint such as `"[::1]:12345"`.
#' @param backlog Maximum pending connection queue length.
#' @param reuse_address Whether to set `SO_REUSEADDR` before binding.
#' @param cloexec Whether to set close-on-exec.
#' @param prefer Address families to try, in order.
#' @return A listening socketR handle.
#' @examples
#' listener <- socket_listen_auto(port = 0L, prefer = "inet")
#' socket_local_name(listener)
#' socket_close(listener)
#' @export
socket_listen_auto <- function(address = NULL, port = NULL, backlog = 128L,
                                reuse_address = TRUE, cloexec = TRUE,
                                prefer = c("inet6", "inet")) {
  endpoint <- parse_auto_endpoint(address, port, allow_null = is.null(address))
  address <- endpoint$address
  port <- endpoint$port
  prefer <- match.arg(prefer, c("inet", "inet6"), several.ok = TRUE)
  candidates <- lapply(prefer, function(domain) {
    if (is.null(address)) {
      list(list(family = domain,
                address = if (domain == "inet6") "::" else "0.0.0.0",
                port = port))
    } else {
      tryCatch(socket_resolve(address, domain, port),
               error = function(error) NULL)
    }
  })
  candidates <- unlist(candidates, recursive = FALSE)
  if (!length(candidates)) {
    stop("could not resolve the local address as an IPv4 or IPv6 endpoint",
         call. = FALSE)
  }

  last_error <- NULL
  for (candidate in candidates) {
    listener <- socket_create(candidate$family, "stream", cloexec = cloexec)
    if (isTRUE(reuse_address)) socket_reuse_address(listener, TRUE)
    result <- tryCatch({
      socket_bind(listener, candidate$address, candidate$port)
      socket_listen(listener, backlog)
      TRUE
    }, error = identity)
    if (isTRUE(result)) return(listener)
    last_error <- result
    socket_close(listener)
  }

  stop(sprintf("could not listen on '%s:%d': %s",
               if (is.null(address)) "*" else address, port,
               conditionMessage(last_error)), call. = FALSE)
}

#' Stream socket I/O.
#'
#' Write bytes to a connected socket.
#' This is a connection-style alias for [socket_send()]. It returns the number
#' of bytes written, which may be less than the input length for a nonblocking
#' socket or a large payload.
#'
#' @rdname socket_io
#' @name socket_io
#' @param socket A connected socket handle.
#' @param object A raw vector or character scalar.
#' @param flags Native `send()` flags.
#' @return Number of bytes written, or `NA_integer_` if I/O would block.
#' @examples
#' if (interactive()) {
#'   server <- socket_create()
#'   client <- socket_create()
#'   socket_bind(server, "127.0.0.1", 0L)
#'   socket_listen(server)
#'   socket_connect(client, "127.0.0.1", socket_local_name(server)$port)
#'   peer <- socket_accept(server)
#'   socket_write(client, "hello")
#'   socket_read(peer, 5L)
#'   socket_close(peer); socket_close(client); socket_close(server)
#' }
#' @export
socket_write <- function(socket, object, flags = 0L) {
  socket_send(socket, object, flags)
}

#' Adapt sockets and R connections for byte I/O.
#'
#' Adapt a socket handle to an R connection.
#' The returned connection can be passed to base R functions such as
#' `readBin()`, `writeBin()`, `readLines()`, and `writeLines()`. Closing the
#' connection does not close the underlying socket handle; call [socket_close()]
#' explicitly when the handle is no longer needed.
#'
#' @rdname socket_connections
#' @name socket_connections
#' @param socket A socketR socket handle.
#' @param close_socket Whether closing the adapter should also close the
#' underlying socket. The default `FALSE` preserves adapter-only close
#' behavior.
#' @return An R `connection` object.
#' @examples
#' s <- socket_create()
#' con <- socket_connection(s)
#' close(con)
#' socket_close(s)
#' @export
socket_connection <- function(socket, close_socket = FALSE) {
  socketr_call(socketr_socket_connection, check_socket(socket), isTRUE(close_socket))
}

#' Read bytes through an R connection.
#'
#' Uses R's connection dispatch, so this works with [socket_connection()] and
#' other readable R connections.
#'
#' @rdname socket_connections
#' @param connection An open R connection.
#' @param n Maximum number of bytes to read.
#' @return A raw vector, possibly shorter than `n` at end-of-file.
#' @export
socket_connection_read <- function(connection, n = 4096L) {
  if (!inherits(connection, "connection")) {
    stop("`connection` must be an R connection", call. = FALSE)
  }
  if (!isOpen(connection)) open(connection, "r+b")
  socketr_call(socketr_connection_read, connection, as.integer(n))
}

#' Write bytes through an R connection.
#'
#' Uses R's connection dispatch, so this works with [socket_connection()] and
#' other writable R connections.
#'
#' @rdname socket_connections
#' @param connection An open R connection.
#' @param data A raw vector.
#' @return Number of bytes written.
#' @export
socket_connection_write <- function(connection, data) {
  if (!inherits(connection, "connection")) {
    stop("`connection` must be an R connection", call. = FALSE)
  }
  if (!is.raw(data)) stop("`data` must be a raw vector", call. = FALSE)
  if (!isOpen(connection)) open(connection, "r+b")
  socketr_call(socketr_connection_write, connection, data)[[1L]]
}

#' Send bytes on a connected socket.
#' @rdname socket_io
#' @param socket A connected socket handle.
#' @param data A raw vector or character scalar.
#' @param flags Native `send()` flags.
#' @return Number of bytes sent, or `NA_integer_` if nonblocking I/O would block.
#' @export
socket_send <- function(socket, data, flags = 0L) {
  socketr_call(socketr_socket_send, check_socket(socket), as_socket_data(data), as.integer(flags))[[1L]]
}

#' Read bytes from a connected socket.
#'
#' This is a connection-style alias for [socket_receive()]. Blocking sockets
#' wait until at least one byte is available or the peer closes the connection.
#'
#' @rdname socket_io
#' @param socket A connected socket handle.
#' @param n Maximum number of bytes to read.
#' @param flags Native `recv()` flags.
#' @return A raw vector, or `NULL` if a nonblocking read would block.
#' @export
socket_read <- function(socket, n = 4096L, flags = 0L) {
  socket_receive(socket, n, flags)
}

#' Receive bytes from a connected socket. Returns NULL when a nonblocking socket would block.
#' @rdname socket_io
#' @param socket A connected socket handle.
#' @param n Maximum number of bytes to receive.
#' @param flags Native `recv()` flags.
#' @return A raw vector, or `NULL` if nonblocking I/O would block.
#' @export
socket_receive <- function(socket, n = 4096L, flags = 0L) {
  socketr_call(socketr_socket_recv, check_socket(socket), as.integer(n), as.integer(flags))
}

#' Datagram socket I/O.
#'
#' Send bytes to a datagram destination.
#' @rdname socket_datagrams
#' @name socket_datagrams
#' @param socket A datagram socket handle.
#' @param data A raw vector or character scalar.
#' @param address Destination hostname, IP address, or Unix-domain path.
#' @param port Destination port; `NULL` for Unix sockets.
#' @param flags Native `sendto()` flags.
#' @return Number of bytes sent, or `NA_integer_` if I/O would block.
#' @examples
#' if (interactive()) {
#'   receiver <- socket_create(type = "dgram")
#'   sender <- socket_create(type = "dgram")
#'   socket_bind(receiver, "127.0.0.1", 0L)
#'   socket_send_to(sender, "hello", "127.0.0.1",
#'                  socket_local_name(receiver)$port)
#'   socket_receive_from(receiver)
#'   socket_close(sender); socket_close(receiver)
#' }
#' @export
socket_send_to <- function(socket, data, address, port = NULL, flags = 0L) {
  target <- normalize_address_port(check_socket(socket), address, port, bind = FALSE)
  socketr_call(socketr_socket_sendto, socket, as_socket_data(data), target$address,
              target$port, as.integer(flags))[[1L]]
}

#' Receive a datagram and its source address. Returns NULL when a nonblocking socket would block.
#' @rdname socket_datagrams
#' @param socket A datagram socket handle.
#' @param n Maximum datagram payload size.
#' @param flags Native `recvfrom()` flags.
#' @return A list with `data` and `address`, or `NULL` if I/O would block.
#' @export
socket_receive_from <- function(socket, n = 4096L, flags = 0L) {
  socketr_call(socketr_socket_recvfrom, check_socket(socket), as.integer(n), as.integer(flags))
}

#' Shut down part of a full-duplex socket.
#' @rdname socket_lifecycle
#' @param socket A socketR socket handle.
#' @param how One of `"read"`, `"write"`, or `"both"`.
#' @return Invisibly, the socket handle.
#' @export
socket_shutdown <- function(socket, how = c("both", "read", "write")) {
  invisible(socketr_call(socketr_socket_shutdown, check_socket(socket), shutdown_how(how)))
}

#' Inspect socket identity and readiness.
#'
#' Return the local socket name.
#' @rdname socket_inspection
#' @name socket_inspection
#' @param socket A socketR socket handle.
#' @return Address metadata as a list.
#' @examples
#' s <- socket_create()
#' socket_local_name(s)
#' socket_close(s)
#' @export
socket_local_name <- function(socket) {
  socketr_call(socketr_socket_name, check_socket(socket), FALSE)
}

#' Return the connected peer socket name.
#' @rdname socket_inspection
#' @param socket A connected socket handle.
#' @return Address metadata as a list.
#' @export
socket_peer_name <- function(socket) {
  socketr_call(socketr_socket_name, check_socket(socket), TRUE)
}

#' Poll sockets for readiness.
#' @rdname socket_readiness
#' @param sockets A socket handle or list of handles.
#' @param events Requested `"read"`, `"write"`, or combined readiness.
#' @param timeout_ms Timeout in milliseconds; use `-1` to wait indefinitely.
#' @return A data frame with readiness flags and native revents values.
#' @examples
#' s <- socket_create()
#' socket_poll(s, "read", timeout_ms = 0L)
#' socket_close(s)
#' @export
socket_poll <- function(sockets, events = "read", timeout_ms = 60000L) {
  if (inherits(sockets, "socketr_socket")) sockets <- list(sockets)
  if (!is.list(sockets)) stop("`sockets` must be a socket handle or list of handles", call. = FALSE)
  sockets <- lapply(sockets, check_socket)

  if (is.list(events)) {
    masks <- vapply(events, events_to_mask, integer(1L))
  } else if (length(events) == length(sockets) && (is.integer(events) || is.numeric(events))) {
    masks <- as.integer(events)
  } else {
    mask <- events_to_mask(events)
    masks <- rep.int(mask, length(sockets))
  }

  result <- socketr_call(socketr_socket_poll, sockets, masks, as.integer(timeout_ms))
  data.frame(
    socket = seq_along(sockets),
    readable = result$readable,
    writable = result$writable,
    error = result$error,
    hangup = result$hangup,
    invalid = result$invalid,
    revents = result$revents,
    stringsAsFactors = FALSE
  )
}

#' Get a socket option.
#' @param socket A socketR socket handle.
#' @param level Option level such as `"socket"` or `"tcp"`.
#' @param option Option name or native numeric constant.
#' @param type Return type: `"int"`, `"logical"`, `"timeval"`, `"linger"`, or `"raw"`.
#' @param size Maximum raw option size.
#' @return The option value.
#' @export
socket_get_option <- function(socket, level = "socket", option, type = c("int", "logical", "timeval", "linger", "raw"), size = 256L) {
  type <- match.arg(type)
  socketr_call(socketr_socket_get_option, check_socket(socket), level, option, type, as.integer(size))
}

#' Set a socket option.
#' @param socket A socketR socket handle.
#' @param level Option level such as `"socket"` or `"tcp"`.
#' @param option Option name or native numeric constant.
#' @param value Value matching `type`.
#' @param type Value type: `"int"`, `"logical"`, `"timeval"`, `"linger"`, or `"raw"`.
#' @return Invisibly, the socket handle.
#' @export
socket_set_option <- function(socket, level = "socket", option, value, type = c("int", "logical", "timeval", "linger", "raw")) {
  type <- match.arg(type)
  invisible(socketr_call(socketr_socket_set_option, check_socket(socket), level, option, value, type))
}

#' Set multiple socket options in order.
#'
#' Each element of `options` must be a list containing `option` and `value`,
#' with optional `level` and `type` entries. Options are applied in list order.
#' If a native option fails, the error includes the zero-based count of options
#' already applied in its `applied` field.
#'
#' @param socket A socketR socket handle.
#' @param options A non-empty list of option specifications.
#' @return The socket handle, invisibly.
#' @export
socket_set_options <- function(socket, options) {
  socket <- check_socket(socket)
  if (!is.list(options) || !length(options)) {
    stop("`options` must be a non-empty list", call. = FALSE)
  }

  specs <- lapply(seq_along(options), function(i) {
    spec <- options[[i]]
    if (!is.list(spec) || is.null(spec$option) || is.null(spec$value)) {
      stop(sprintf("option specification %d must contain `option` and `value`", i),
           call. = FALSE)
    }
    type <- if (is.null(spec$type)) "int" else
      match.arg(spec$type, c("int", "logical", "timeval", "linger", "raw"))
    list(
      level = if (is.null(spec$level)) "socket" else spec$level,
      option = spec$option,
      value = spec$value,
      type = type
    )
  })

  applied <- integer()
  for (i in seq_along(specs)) {
    result <- tryCatch(
      socket_set_option(socket, specs[[i]]$level, specs[[i]]$option,
                        specs[[i]]$value, specs[[i]]$type),
      error = function(error) {
        condition <- structure(
          list(
            message = sprintf("socket option batch failed at option %d: %s",
                              i, conditionMessage(error)),
            call = NULL,
            cause = error,
            failed = i,
            applied = applied
          ),
          class = c("socketr_option_batch_error", "error", "condition")
        )
        stop(condition)
      }
    )
    applied <- c(applied, i)
  }
  invisible(socket)
}

#' Common socket option helpers.
#'
#' These helpers read or set frequently used socket options. With the `value`
#' argument omitted, the current option value is returned; otherwise the
#' option is updated and the socket handle is returned invisibly. The
#' `socket_linger()` helper uses `on` and `seconds` to configure `SO_LINGER`.
#'
#' @rdname socket_options
#' @name socket_options
#' @param socket A socketR socket handle.
#' @param value Optional option value to set. Its type depends on the helper:
#' logical, integer, or numeric seconds.
#' @param on Optional logical linger enable flag.
#' @param seconds Linger duration in seconds.
#' @return The option value when reading; invisibly, the socket when setting.
#' @examples
#' s <- socket_create()
#' socket_reuse_address(s, TRUE)
#' socket_reuse_address(s)
#' socket_close(s)
#' @export
socket_reuse_address <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "reuseaddr", "logical"))
  socket_set_option(socket, "socket", "reuseaddr", isTRUE(value), "logical")
}

#' Get or set `SO_KEEPALIVE`.
#' @rdname socket_options
#' @export
socket_keep_alive <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "keepalive", "logical"))
  socket_set_option(socket, "socket", "keepalive", isTRUE(value), "logical")
}

#' Get or set `SO_BROADCAST`.
#' @rdname socket_options
#' @export
socket_broadcast <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "broadcast", "logical"))
  socket_set_option(socket, "socket", "broadcast", isTRUE(value), "logical")
}

#' Get or set `TCP_NODELAY`.
#' @rdname socket_options
#' @export
socket_no_delay <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "tcp", "nodelay", "logical"))
  socket_set_option(socket, "tcp", "nodelay", isTRUE(value), "logical")
}

#' Get or set the Linux TCP_QUICKACK option.
#'
#' @rdname socket_options
#' @export
socket_quick_ack <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "tcp", "quickack", "logical"))
  socket_set_option(socket, "tcp", "quickack", isTRUE(value), "logical")
}

#' Get or set `SO_RCVBUF`.
#' @rdname socket_options
#' @export
socket_receive_buffer_size <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "rcvbuf", "int"))
  socket_set_option(socket, "socket", "rcvbuf", as.integer(value), "int")
}

#' Get or set `SO_SNDBUF`.
#' @rdname socket_options
#' @export
socket_send_buffer_size <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "sndbuf", "int"))
  socket_set_option(socket, "socket", "sndbuf", as.integer(value), "int")
}

#' Get or set `SO_RCVTIMEO`.
#' @rdname socket_options
#' @export
socket_receive_timeout <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "rcvtimeo", "timeval"))
  socket_set_option(socket, "socket", "rcvtimeo", as.numeric(value), "timeval")
}

#' Get or set `SO_SNDTIMEO`.
#' @rdname socket_options
#' @export
socket_send_timeout <- function(socket, value) {
  if (missing(value)) return(socket_get_option(socket, "socket", "sndtimeo", "timeval"))
  socket_set_option(socket, "socket", "sndtimeo", as.numeric(value), "timeval")
}

#' Get or set `SO_LINGER`.
#' @rdname socket_options
#' @param on Optional logical linger enable flag.
#' @param seconds Linger duration in seconds.
#' @export
socket_linger <- function(socket, on, seconds = 0L) {
  if (missing(on)) return(socket_get_option(socket, "socket", "linger", "linger"))
  socket_set_option(socket, "socket", "linger", list(on = isTRUE(on), seconds = as.integer(seconds)), "linger")
}
