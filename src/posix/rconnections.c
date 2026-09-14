// Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

#include "rconnections.h"
#include <R_ext/Connections.h>

#if !defined(R_CONNECTIONS_VERSION) || R_CONNECTIONS_VERSION != 1
#error "socketR requires R_CONNECTIONS_VERSION == 1"
#endif

SEXP socketr_new_custom_connection(const char* description, const char* mode,
                                   const char* class_name, Rconnection* ptr) {
  return R_new_custom_connection(description, mode, class_name, ptr);
}

void socketr_connection_set_callbacks(
    Rconnection connection, socketr_connection_open_fn open,
    socketr_connection_void_fn close, socketr_connection_void_fn destroy,
    socketr_connection_fgetc_fn fgetc,
    socketr_connection_fgetc_fn fgetc_internal,
    socketr_connection_read_fn read, socketr_connection_write_fn write,
    socketr_connection_fflush_fn fflush,
    socketr_connection_seek_fn seek) {
  connection->open = open;
  connection->close = close;
  connection->destroy = destroy;
  connection->fgetc = fgetc;
  connection->fgetc_internal = fgetc_internal;
  connection->read = read;
  connection->write = write;
  connection->fflush = fflush;
  connection->seek = seek;
}

void socketr_connection_set_flags(Rconnection connection, Rboolean canread,
                                  Rboolean canwrite, Rboolean canseek,
                                  Rboolean text, Rboolean blocking,
                                  Rboolean isopen) {
  connection->canread = canread;
  connection->canwrite = canwrite;
  connection->canseek = canseek;
  connection->text = text;
  connection->blocking = blocking;
  connection->isopen = isopen;
}

void* socketr_connection_get_ex_ptr(Rconnection connection) {
  return connection->ex_ptr;
}

void socketr_connection_set_ex_ptr(Rconnection connection, void* ex_ptr) {
  connection->ex_ptr = ex_ptr;
}

void socketr_connection_set_isopen(Rconnection connection, Rboolean isopen) {
  connection->isopen = isopen;
}
