# Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' POSIX socket programming for R
#'
#' External-pointer sockets implemented with base R's C API for POSIX TCP, UDP, IPv4, and IPv6
#' and Unix-domain socket operations, including bind, listen, accept, connect,
#' send, receive, polling, shutdown, close, socket names, and typed options.
#'
#' Socket handles are RAII-managed external pointers and should still be
#' closed explicitly with [socket_close()] when possible. Most system-call
#' failures include the POSIX errno name and number.
#'
#' @keywords package
#' @importFrom R6 R6Class
#' @useDynLib socketR, .registration = TRUE
"_PACKAGE"
