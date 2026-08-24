// Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

#include <R_ext/Rdynload.h>

extern "C" void R_init_socketR(DllInfo* dll) {
  R_useDynamicSymbols(dll, FALSE);
}
