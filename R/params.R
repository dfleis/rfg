#' @include rfg-classes.R
NULL

#' @title Extract Parameters from an RFG Function
#'
#' @description
#' An S7 generic to extract the `list` of `<rfg_params>` from a generated
#' `<rfg_function>` object.
#'
#' @param x An object of class `<rfg_function>`.
#' @param ... Other arguments passed to `params()`. Currently ignored.
#'
#' @return A `list` of `<rfg_params>` objects.
#' 
#' @export
params <- S7::new_generic(
  name = "params", 
  dispatch_args = "x",
  fun = function(x, ...) S7::S7_dispatch()
)

#' @export
S7::method(params, rfg_function) <- function(x, ...) {
  x@params
}