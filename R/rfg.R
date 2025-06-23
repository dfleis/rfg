#' @include classes.R
NULL

#' @title Create a RFG Function
#'
#' @description
#' Creates a RFG function by either randomly generating the RFG parameters, or
#' by supplying a `list` of `<rfg_params>` objects, each specifying the
#' component functions of a vector-valued RFG function.
#'
#' @details
#' This function is an S7 generic that can be called as `rfg(p, ...)` in the
#' following ways:
#' \itemize{
#'  \item Automatically generating parameters: When `p` is a positive integer,
#'    call `rfg(p, ...)` to generate a new RFG function whose domain (input)
#'    has dimensionality `p`.
#'  \item Pre-specified parameters: When `p` is a `list` of `<rfg_params>`
#'    objects, call `rfg(p)` to create the corresponding RFG function.
#' }
#'
#' @param p The primary dispatch argument. Can be either:
#'   \itemize{
#'     \item A positive integer indicating the desired input dimensionality `p`.
#'     \item A `list` of `<rfg_params>`, each representing the parameters for
#'      an individual scalar-valued RFG function. The dimensionality of the RFG
#'      function's range is implied by the length of the `<rfg_params>` list.
#'   }
#' @param q When `p` is an integer value, `q` is the desired dimensionality
#'    of the RFG function's range (output). Defaults to `q = 1L` and generates
#'    a scalar-valued RFG function. Otherwise, when `p` is a `<rfg_params>`
#'    object, `q` is ignored.
#' @param ... When `p` is an integer value, these are optional arguments
#'    passed to [generate_rfg_params()]. Otheriwse ignored.
#' @param seed When `p` is an integer value, an optional seed may be supplied
#'    for reproducible parameter generation. See the internal function
#'    [with_optional_seed()] for details.
#'
#' @return An object of class `<rfg_function>`.
#' 
#' @seealso [generate_rfg_params()], [with_optional_seed()]
#' @template friedman-reference
#' 
#' @export
rfg <- S7::new_generic(
  name = "rfg", 
  dispatch_args = "p", 
  fun = function(p, q = 1L, ..., seed = NULL) S7::S7_dispatch()
)

#' @export
S7::method(rfg, S7::class_list) <- function(p, q = 1L, ..., seed = NULL) {
  param_list <- p # p is a list of <rfg_params> objects
  
  .create_scalar_rfg_fun <- function(scalar_rfg_fun_params) {
    # TODO Should we call `force(scalar_rfg_fun_params@p)` and
    # `force(scalar_rfg_fun_params@bases)` to ensure these things are properly
    # captured before the inner function definition?
    p_dim <- scalar_rfg_fun_params@p
    bases <- scalar_rfg_fun_params@bases
    
    FUN <- function(x) {
      if (!isTRUE(NCOL(x) == p_dim)) {
        stop(sprintf("RFG function inputs must have %i columns", p_dim))
      }
      res <- lapply(bases, function(b) { 
        # TODO Probably a nicer way to do this. That said, this definition
        # is pretty fast when computing values at computation-time, particularly
        # since @phi, @mu, and @V tend to be fairly small. The smallness is by
        # design. Their dimension is controlled by an exponential random
        # variable with (default) mean 2, and thus we expect their dimension to
        # be between 3 and 4. Obviously, if the dimension of x is very large,
        # or if the user manually specifies a larger exponential rate, then
        # this could blow up.
        zc <- sweep(x[, b@phi, drop = FALSE], 2, b@mu, "-") 
        zV <- zc %*% b@V
        b@a * exp(-0.5 * rowSums(zV * zc))
      })
      Reduce(`+`, res)
    }
    return(FUN)
  }
  
  # TODO Is it a good idea to call `force` here to ensure these functions
  # are themselves captured before we create the later function?
  FUNS <- lapply(param_list, .create_scalar_rfg_fun)
  
  FUN <- function(x) {
    vapply(FUNS, function(f) f(x), FUN.VALUE = numeric(NROW(x)))
  }
  
  rfg_function(FUN, params = param_list)
}

#' @export
S7::method(rfg, S7::class_numeric) <- function(p, q = 1L, ..., seed = NULL) {
  param_list <- with_optional_seed(
    code = generate_rfg_params(p = p, q = q, ...),
    seed = seed
  )
  
  rfg(param_list)
}

