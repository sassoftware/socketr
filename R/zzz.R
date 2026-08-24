# Copyright © 2026, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

.onAttach <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows") {
    packageStartupMessage(
      "socketR supports Linux/POSIX systems only; Windows is not supported."
    )
  }
}
