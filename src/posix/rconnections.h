// Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

#ifndef SOCKETR_RCONNECTIONS_H
#define SOCKETR_RCONNECTIONS_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Rconn* Rconnection;

Rconnection R_GetConnection(SEXP sConn);
size_t R_ReadConnection(Rconnection con, void* buf, size_t n);
size_t R_WriteConnection(Rconnection con, void* buf, size_t n);

typedef Rboolean (*socketr_connection_open_fn)(Rconnection);
typedef void (*socketr_connection_void_fn)(Rconnection);
typedef int (*socketr_connection_fgetc_fn)(Rconnection);
typedef size_t (*socketr_connection_read_fn)(void*, size_t, size_t,
                                             Rconnection);
typedef size_t (*socketr_connection_write_fn)(const void*, size_t, size_t,
                                              Rconnection);
typedef int (*socketr_connection_fflush_fn)(Rconnection);
typedef double (*socketr_connection_seek_fn)(Rconnection, double, int, int);

SEXP socketr_new_custom_connection(const char* description, const char* mode,
                                   const char* class_name, Rconnection* ptr);
void socketr_connection_set_callbacks(
    Rconnection connection, socketr_connection_open_fn open,
    socketr_connection_void_fn close, socketr_connection_void_fn destroy,
    socketr_connection_fgetc_fn fgetc,
    socketr_connection_fgetc_fn fgetc_internal,
    socketr_connection_read_fn read, socketr_connection_write_fn write,
    socketr_connection_fflush_fn fflush,
    socketr_connection_seek_fn seek);
void socketr_connection_set_flags(Rconnection connection, Rboolean canread,
                                  Rboolean canwrite, Rboolean canseek,
                                  Rboolean text, Rboolean blocking,
                                  Rboolean isopen);
void* socketr_connection_get_ex_ptr(Rconnection connection);
void socketr_connection_set_ex_ptr(Rconnection connection, void* ex_ptr);
void socketr_connection_set_isopen(Rconnection connection, Rboolean isopen);

#ifdef __cplusplus
}
#endif

#endif
