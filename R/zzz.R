# nocov start
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# nocov end