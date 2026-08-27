# Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

skip_if(.Platform$OS.type == "windows")

test_that("socket handles expose lifecycle and options", {
  s <- socket_create("inet", "stream")
  on.exit(if (socket_is_open(s)) socket_close(s), add = TRUE)

  expect_true(inherits(s, "socketr_socket"))
  expect_true(socket_is_open(s))
  expect_equal(socket_info(s)$domain, "inet")
  expect_equal(s$family, "inet")
  expect_equal(s$type, "stream")
  expect_true(isTRUE(s$open))
  expect_true(isTRUE(s$blocking))

  socket_reuse_address(s, TRUE)
  expect_true(socket_reuse_address(s))

  socket_set_blocking(s, FALSE)
  expect_false(socket_info(s)$blocking)
  socket_set_blocking(s, TRUE)
  expect_true(socket_info(s)$blocking)

  socket_close(s)
  expect_false(socket_is_open(s))
  expect_true(is.na(socket_fd(s)))
  expect_false(isTRUE(s$open))
})

test_that("socket attributes expose endpoint metadata and R6 active bindings", {
  server <- Socket$new("inet", "stream")
  client <- Socket$new("inet", "stream")
  peer <- NULL
  on.exit({
    if (!is.null(peer)) peer$close()
    client$close()
    server$close()
  }, add = TRUE)

  server$bind("127.0.0.1", 0L)
  server$listen()
  client$connect("127.0.0.1", server$port)
  peer <- server$accept()

  expect_equal(server$address, "127.0.0.1")
  expect_gt(server$port, 0L)
  expect_equal(client$peer_address, "127.0.0.1")
  expect_equal(client$peer_port, server$port)
  expect_equal(peer$peer_address, "127.0.0.1")
  expect_equal(peer$peer_port, client$port)
})

test_that("R6 auto constructors create connected and listening sockets", {
  server <- Socket$new_listener(port = 0L, prefer = "inet")
  on.exit(server$close(), add = TRUE)
  client <- Socket$new_auto(server$address, server$port, prefer = "inet")
  on.exit(client$close(), add = TRUE)
  peer <- server$accept()
  on.exit(peer$close(), add = TRUE)
  expect_true(client$is_open())
  expect_equal(client$peer_port, server$port)
})

test_that("socket_resolve returns system resolver endpoints", {
  resolved <- socket_resolve("localhost", "inet", 8080L)
  expect_true(length(resolved) >= 1L)
  expect_true(all(vapply(resolved, function(x) x$family == "inet", logical(1))))
  expect_true(all(vapply(resolved, function(x) x$port == 8080L, logical(1))))
})

test_that("socket options can be applied as an ordered batch", {
  s <- socket_create("inet", "stream")
  on.exit(if (socket_is_open(s)) socket_close(s), add = TRUE)

  socket_set_options(s, list(
    list(level = "socket", option = "reuseaddr", value = TRUE, type = "logical"),
    list(level = "socket", option = "keepalive", value = TRUE, type = "logical")
  ))
  expect_true(socket_reuse_address(s))
  expect_true(socket_keep_alive(s))

  failure <- tryCatch(
    socket_set_options(s, list(
      list(option = "reuseaddr", value = FALSE, type = "logical"),
      list(level = "tcp", option = "unsupported-option", value = TRUE, type = "logical")
    )),
    error = identity
  )
  expect_s3_class(failure, "socketr_option_batch_error")
  expect_equal(failure$failed, 2L)
  expect_equal(failure$applied, 1L)
})

test_that("TCP loopback sockets send, receive, and poll", {
  server <- socket_create("inet", "stream")
  client <- socket_create("inet", "stream")
  accepted <- NULL
  on.exit({
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_reuse_address(server, TRUE)
  socket_bind(server, "127.0.0.1", 0L)
  port <- socket_local_name(server)$port
  socket_listen(server, 16L)

  expect_true(socket_connect(client, "127.0.0.1", port))
  socket_no_delay(client, TRUE)
  expect_true(socket_no_delay(client))

  accepted <- socket_accept(server)
  expect_true(inherits(accepted, "socketr_socket"))
  expect_equal(socket_peer_name(client)$port, port)

  expect_equal(socket_write(client, "ping"), 4L)
  ready <- socket_poll(accepted, "read", 1000L)
  expect_true(ready$readable[[1L]])
  expect_identical(rawToChar(socket_read(accepted, 4L)), "ping")

  expect_equal(socket_send(accepted, "pong"), 4L)
  ready <- socket_poll(client, "read", 1000L)
  expect_true(ready$readable[[1L]])
  expect_identical(rawToChar(socket_receive(client, 4L)), "pong")
})

test_that("hostnames resolve through the system resolver", {
  server <- socket_create("inet", "stream")
  client <- socket_create("inet", "stream")
  accepted <- NULL
  on.exit({
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, "localhost", 0L)
  port <- socket_local_name(server)$port
  socket_listen(server)
  expect_true(socket_connect(client, "localhost", port))
  accepted <- socket_accept(server)
  expect_true(inherits(accepted, "socketr_socket"))
})

test_that("automatic connection selects an available address family", {
  server <- socket_create("inet", "stream")
  on.exit(socket_close(server), add = TRUE)
  socket_bind(server, "127.0.0.1", 0L)
  port <- socket_local_name(server)$port
  socket_listen(server)

  client <- socket_connect_auto("127.0.0.1", port, prefer = c("inet6", "inet"))
  on.exit(socket_close(client), add = TRUE)
  peer <- socket_accept(server)
  on.exit(socket_close(peer), add = TRUE)
  expect_true(socket_is_open(client))
  expect_true(socket_is_open(peer))
})

test_that("automatic listener selects an available address family", {
  listener <- socket_listen_auto(port = 0L, prefer = c("inet6", "inet"))
  on.exit(socket_close(listener), add = TRUE)
  expect_true(socket_is_open(listener))
  expect_true(socket_local_name(listener)$port > 0L)
})

test_that("automatic helpers accept bracketed IPv6 endpoints", {
  ipv6_available <- tryCatch({
    probe <- socket_create("inet6", "stream")
    socket_bind(probe, "::1", 0L)
    socket_close(probe)
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ipv6_available, "IPv6 loopback is unavailable")

  listener <- socket_listen_auto("[::1]", port = 0L, prefer = "inet6")
  on.exit(socket_close(listener), add = TRUE)
  endpoint <- sprintf("[::1]:%d", listener$port)
  client <- socket_connect_auto(endpoint, prefer = "inet6")
  on.exit(socket_close(client), add = TRUE)
  peer <- socket_accept(listener)
  on.exit(socket_close(peer), add = TRUE)
  expect_equal(client$peer_address, "::1")
})

test_that("automatic helpers accept host and IPv4 port forms", {
  listener <- socket_listen_auto("127.0.0.1:0", prefer = "inet")
  on.exit(socket_close(listener), add = TRUE)
  client <- socket_connect_auto(
    sprintf("127.0.0.1:%d", listener$port), prefer = "inet"
  )
  on.exit(socket_close(client), add = TRUE)
  peer <- socket_accept(listener)
  on.exit(socket_close(peer), add = TRUE)
  expect_equal(client$peer_address, "127.0.0.1")
})

test_that("regular address-taking functions accept endpoint notation", {
  server <- socket_create("inet", "stream")
  client <- socket_create("inet", "stream")
  peer <- NULL
  on.exit({
    if (!is.null(peer) && socket_is_open(peer)) socket_close(peer)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, "127.0.0.1:0")
  port <- server$port
  socket_listen(server)
  socket_connect(client, sprintf("127.0.0.1:%d", port))
  peer <- socket_accept(server)
  expect_true(socket_is_open(peer))
})

test_that("socket handles adapt to base R connections", {
  server <- socket_create("inet", "stream")
  client <- socket_create("inet", "stream")
  accepted <- NULL
  server_connection <- NULL
  client_connection <- NULL
  on.exit({
    if (!is.null(server_connection) && isOpen(server_connection)) close(server_connection)
    if (!is.null(client_connection) && isOpen(client_connection)) close(client_connection)
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, "127.0.0.1", 0L)
  port <- socket_local_name(server)$port
  socket_listen(server)
  socket_connect(client, "127.0.0.1", port)
  accepted <- socket_accept(server)
  client_connection <- socket_connection(client)
  server_connection <- socket_connection(accepted)

  writeBin(charToRaw("connection"), client_connection)
  expect_true(socket_poll(accepted, "read", 1000L)$readable[[1L]])
  expect_identical(rawToChar(readBin(server_connection, "raw", 10L)), "connection")
})

test_that("connection ownership remains adapter-only by default or closes socket", {
  adapter_only <- socket_create()
  adapter <- socket_connection(adapter_only)
  close(adapter)
  expect_true(socket_is_open(adapter_only))
  socket_close(adapter_only)

  owned <- socket_create()
  owned_adapter <- socket_connection(owned, close_socket = TRUE)
  close(owned_adapter)
  expect_false(socket_is_open(owned))
})

test_that("connection helpers use R connection dispatch", {
  server <- socket_create()
  client <- socket_create()
  accepted <- NULL
  on.exit({
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, "127.0.0.1", 0L)
  port <- socket_local_name(server)$port
  socket_listen(server)
  socket_connect(client, "127.0.0.1", port)
  accepted <- socket_accept(server)

  client_connection <- socket_connection(client)
  server_connection <- socket_connection(accepted)
  on.exit(close(client_connection), add = TRUE)
  on.exit(close(server_connection), add = TRUE)

  expect_equal(socket_connection_write(client_connection, charToRaw("hello")), 5L)
  expect_identical(socket_connection_read(server_connection, 5L), charToRaw("hello"))
})

test_that("IPv6 loopback sockets send and receive", {
  ipv6_available <- tryCatch({
    probe <- socket_create("inet6", "stream")
    socket_bind(probe, "::1", 0L)
    socket_close(probe)
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ipv6_available, "IPv6 loopback is unavailable")

  server <- socket_create("inet6", "stream")
  client <- socket_create("inet6", "stream")
  accepted <- NULL
  on.exit({
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  expect_no_error(socket_bind(server, "::1", 0L))
  port <- socket_local_name(server)$port
  socket_listen(server)
  expect_true(socket_connect(client, "::1", port))
  accepted <- socket_accept(server)
  expect_equal(socket_send(client, "v6"), 2L)
  expect_true(socket_poll(accepted, "read", 1000L)$readable[[1L]])
  expect_identical(rawToChar(socket_receive(accepted, 2L)), "v6")
})

test_that("UDP loopback sendto and recvfrom work", {
  server <- socket_create("inet", "dgram")
  client <- socket_create("inet", "dgram")
  on.exit({
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, "127.0.0.1", 0L)
  port <- socket_local_name(server)$port

  expect_equal(socket_send_to(client, "hello", "127.0.0.1", port), 5L)
  ready <- socket_poll(server, "read", 1000L)
  expect_true(ready$readable[[1L]])

  msg <- socket_receive_from(server, 64L)
  expect_identical(rawToChar(msg$data), "hello")
  expect_equal(msg$address$family, "inet")
  expect_gt(msg$address$port, 0L)
})

test_that("Unix-domain stream sockets work on POSIX paths", {
  path <- file.path(".", sprintf("socketr-%d.sock", Sys.getpid()))
  skip_if(nchar(normalizePath(dirname(path), mustWork = TRUE)) + nchar(basename(path)) + 1L >= 100L,
          "Unix-domain socket path would be too long")
  unlink(path)
  on.exit(unlink(path), add = TRUE)

  server <- socket_create("unix", "stream")
  client <- socket_create("unix", "stream")
  accepted <- NULL
  on.exit({
    if (!is.null(accepted) && socket_is_open(accepted)) socket_close(accepted)
    if (socket_is_open(client)) socket_close(client)
    if (socket_is_open(server)) socket_close(server)
  }, add = TRUE)

  socket_bind(server, path)
  socket_listen(server)
  expect_true(socket_connect(client, path))
  accepted <- socket_accept(server)

  expect_equal(socket_send(client, "u"), 1L)
  expect_identical(rawToChar(socket_receive(accepted, 1L)), "u")
})
