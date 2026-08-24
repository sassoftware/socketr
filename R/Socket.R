# Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' R6 convenience wrapper for socketR handles.
#'
#' The public methods mirror the functional API: `is_open()`, `fd()`,
#' `info()`, `set_blocking()`, `bind()`, `listen()`, `accept()`, `connect()`,
#' `send()`, `receive()`, `send_to()`, `receive_from()`, `shutdown()`,
#' `close()`, `local_name()`, `peer_name()`, `poll()`, `get_option()`,
#' `set_option()`, and `set_options()`. They delegate to the corresponding
#' documented functions while retaining the handle as object state.
#'
#' @field handle The underlying `socketr_socket` external pointer.
#' @field address Local address, when bound.
#' @field port Local port, when bound.
#' @field peer_address Connected peer address, when connected.
#' @field peer_port Connected peer port, when connected.
#' @field family Address family (`"inet"`, `"inet6"`, or `"unix"`).
#' @field type Socket type (`"stream"` or `"dgram"`).
#' @field protocol Numeric socket protocol.
#' @field blocking Whether the socket is in blocking mode.
#' @field open Whether the underlying socket is open.
#' @details
#' Use `Socket$new_auto()` to create a connected client with IPv4/IPv6
#' fallback, or `Socket$new_listener()` to create a listening server with
#' IPv4/IPv6 fallback.
#' @param domain Address family passed to [socket_create()].
#' @param type Socket type passed to [socket_create()].
#' @param protocol Numeric protocol passed to [socket_create()].
#' @param nonblocking Whether the new socket is nonblocking.
#' @param cloexec Whether the new socket is close-on-exec.
#' @param handle An existing socketR handle to wrap.
#' @return An R6 `Socket` object.
#' @export
Socket <- R6::R6Class(
  "Socket",
  public = list(
    handle = NULL,

    #' @description Create a socket wrapper or wrap an existing handle.
    #' @param domain Address family passed to [socket_create()].
    #' @param type Socket type passed to [socket_create()].
    #' @param protocol Numeric protocol passed to [socket_create()].
    #' @param nonblocking Whether the new socket is nonblocking.
    #' @param cloexec Whether the new socket is close-on-exec.
    #' @param handle An existing socketR handle to wrap.
    initialize = function(domain = c("inet", "inet6", "unix"), type = c("stream", "dgram"),
                          protocol = 0L, nonblocking = FALSE, cloexec = TRUE, handle = NULL) {
      if (is.null(handle)) {
        self$handle <- socket_create(domain, type, protocol, nonblocking, cloexec)
      } else {
        self$handle <- check_socket(handle)
      }
    },

    #' @description Test whether the wrapped socket is open.
    is_open = function() socket_is_open(self$handle),
    #' @description Return the wrapped socket file descriptor.
    fd = function() socket_fd(self$handle),
    #' @description Return metadata for the wrapped socket.
    info = function() socket_info(self$handle),

    #' @description Set blocking mode.
    #' @param blocking Whether operations should block.
    set_blocking = function(blocking = TRUE) {
      socket_set_blocking(self$handle, blocking)
      invisible(self)
    },

    #' @description Bind the wrapped socket.
    #' @param address Local hostname, IP address, or Unix path.
    #' @param port Local port, or `NULL` for Unix sockets.
    bind = function(address = NULL, port = NULL) {
      socket_bind(self$handle, address, port)
      invisible(self)
    },

    #' @description Listen for stream connections.
    #' @param backlog Maximum pending connection queue length.
    listen = function(backlog = 128L) {
      socket_listen(self$handle, backlog)
      invisible(self)
    },

    #' @description Accept a pending connection.
    #' @param nonblocking Whether the accepted socket should be nonblocking.
    accept = function(nonblocking = NULL) {
      handle <- socket_accept(self$handle, nonblocking)
      if (is.null(handle)) return(NULL)
      Socket$new(handle = handle)
    },

    #' @description Connect the wrapped socket.
    #' @param address Remote hostname, IP address, or Unix path.
    #' @param port Remote port, or `NULL` for Unix sockets.
    connect = function(address, port = NULL) socket_connect(self$handle, address, port),
    #' @description Return the wrapped socket as an R connection.
    as_connection = function() socket_connection(self$handle),
    #' @description Write bytes to the wrapped socket.
    #' @param object Raw bytes or a character scalar.
    #' @param flags Native `send()` flags.
    write = function(object, flags = 0L) socket_write(self$handle, object, flags),
    #' @description Read bytes from the wrapped socket.
    #' @param n Maximum number of bytes.
    #' @param flags Native `recv()` flags.
    read = function(n = 4096L, flags = 0L) socket_read(self$handle, n, flags),
    #' @description Send bytes on the wrapped socket.
    #' @param data Raw bytes or a character scalar.
    #' @param flags Native `send()` flags.
    send = function(data, flags = 0L) socket_send(self$handle, data, flags),
    #' @description Receive bytes from the wrapped socket.
    #' @param n Maximum number of bytes.
    #' @param flags Native `recv()` flags.
    receive = function(n = 4096L, flags = 0L) socket_receive(self$handle, n, flags),

    #' @description Send a datagram.
    #' @param data Raw bytes or a character scalar.
    #' @param address Destination hostname or IP address.
    #' @param port Destination port.
    #' @param flags Native `sendto()` flags.
    send_to = function(data, address, port = NULL, flags = 0L) {
      socket_send_to(self$handle, data, address, port, flags)
    },

    #' @description Receive a datagram.
    #' @param n Maximum datagram payload size.
    #' @param flags Native `recvfrom()` flags.
    receive_from = function(n = 4096L, flags = 0L) socket_receive_from(self$handle, n, flags),

    #' @description Shut down part of the connection.
    #' @param how One of `"read"`, `"write"`, or `"both"`.
    shutdown = function(how = c("both", "read", "write")) {
      socket_shutdown(self$handle, how)
      invisible(self)
    },

    #' @description Close the wrapped socket.
    close = function() {
      if (!is.null(self$handle) && socket_is_open(self$handle)) socket_close(self$handle)
      invisible(self)
    },

    #' @description Return the local socket name.
    local_name = function() socket_local_name(self$handle),
    #' @description Return the peer socket name.
    peer_name = function() socket_peer_name(self$handle),
    #' @description Poll the wrapped socket.
    #' @param events Requested readiness events.
    #' @param timeout_ms Timeout in milliseconds.
    poll = function(events = "read", timeout_ms = 60000L) socket_poll(self$handle, events, timeout_ms),
    #' @description Get a socket option.
    #' @param level Option level.
    #' @param option Option name or numeric constant.
    #' @param type Return type.
    #' @param size Maximum raw option size.
    get_option = function(level = "socket", option, type = c("int", "logical", "timeval", "linger", "raw"), size = 256L) {
      socket_get_option(self$handle, level, option, type, size)
    },
    #' @description Set multiple socket options.
    #' @param options Option specifications.
    set_options = function(options) {
      socket_set_options(self$handle, options)
      invisible(self)
    },
    #' @description Set a socket option.
    #' @param level Option level.
    #' @param option Option name or numeric constant.
    #' @param value Option value.
    #' @param type Value type.
    set_option = function(level = "socket", option, value,
                          type = c("int", "logical", "timeval", "linger", "raw")) {
      socket_set_option(self$handle, level, option, value, type)
      invisible(self)
    }
  ),
  active = list(
    address = function(value) {
      if (!missing(value)) stop("`address` is read-only", call. = FALSE)
      attr(self$handle, "address", exact = TRUE)
    },
    port = function(value) {
      if (!missing(value)) stop("`port` is read-only", call. = FALSE)
      attr(self$handle, "port", exact = TRUE)
    },
    peer_address = function(value) {
      if (!missing(value)) stop("`peer_address` is read-only", call. = FALSE)
      attr(self$handle, "peer_address", exact = TRUE)
    },
    peer_port = function(value) {
      if (!missing(value)) stop("`peer_port` is read-only", call. = FALSE)
      attr(self$handle, "peer_port", exact = TRUE)
    },
    family = function(value) {
      if (!missing(value)) stop("`family` is read-only", call. = FALSE)
      attr(self$handle, "family", exact = TRUE)
    },
    type = function(value) {
      if (!missing(value)) stop("`type` is read-only", call. = FALSE)
      attr(self$handle, "type", exact = TRUE)
    },
    protocol = function(value) {
      if (!missing(value)) stop("`protocol` is read-only", call. = FALSE)
      attr(self$handle, "protocol", exact = TRUE)
    },
    blocking = function(value) {
      if (!missing(value)) stop("`blocking` is read-only", call. = FALSE)
      attr(self$handle, "blocking", exact = TRUE)
    },
    open = function(value) {
      if (!missing(value)) stop("`open` is read-only", call. = FALSE)
      attr(self$handle, "open", exact = TRUE)
    }
  ),
  private = list(
    finalize = function() {
      if (!is.null(self$handle) && socket_is_open(self$handle)) socket_close(self$handle)
      invisible(self)
    }
  )
)

#' Close an R6 Socket with the standard R connection API.
#'
#' @param con A `Socket` R6 object.
#' @param ... Ignored.
#' @export
close.Socket <- function(con, ...) {
  con$close()
}

#' Create an R6 client using IPv4/IPv6 endpoint fallback.
#'
#' @param address Hostname, IP address, or endpoint such as `"[::1]:8080"`.
#' @param port Optional port when it is not embedded in `address`.
#' @param type Socket type, `"stream"` (TCP) or `"dgram"` (UDP).
#' @param protocol Numeric protocol, usually zero.
#' @param nonblocking Whether the socket should be nonblocking.
#' @param cloexec Whether to set close-on-exec.
#' @param prefer Address families to try, in order.
#' @return A connected [Socket] R6 object.
#' @name Socket-new_auto
#' @rdname Socket-new_auto
Socket$new_auto <- function(address, port = NULL, type = c("stream", "dgram"),
                            protocol = 0L, nonblocking = FALSE,
                            cloexec = TRUE, prefer = c("inet6", "inet")) {
  Socket$new(handle = socket_connect_auto(
    address, port, type, protocol, nonblocking, cloexec, prefer
  ))
}

#' Create an R6 listener using IPv4/IPv6 endpoint fallback.
#'
#' @param address Local hostname, IP address, endpoint, or `NULL` for wildcard.
#' @param port Optional port when it is not embedded in `address`.
#' @param backlog Maximum pending connection queue length.
#' @param reuse_address Whether to set `SO_REUSEADDR`.
#' @param cloexec Whether to set close-on-exec.
#' @param prefer Address families to try, in order.
#' @return A listening [Socket] R6 object.
#' @name Socket-new_listener
#' @rdname Socket-new_listener
Socket$new_listener <- function(address = NULL, port = NULL, backlog = 128L,
                                 reuse_address = TRUE, cloexec = TRUE,
                                 prefer = c("inet6", "inet")) {
  Socket$new(handle = socket_listen_auto(
    address, port, backlog, reuse_address, cloexec, prefer
  ))
}
