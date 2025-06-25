#' @include S7-properties.R
NULL

#' @title The `rfg_basis_params` Class
#'
#' @description
#' An S7 class to represent a parameter set of an individual basis function.
#' 
#' @keywords internal
#' @export
rfg_basis_params <- S7::new_class(
  name = "rfg_basis_params",
  properties = list(
    a   = prop_num_scalar,
    phi = prop_int_vector,
    mu  = prop_num_vector,
    V   = prop_num_sq_matrix
  ),
  validator = function(self) {
    if (length(self@phi) != length(self@mu)) {
      "The length of @phi must be equal to the length of @mu"
    } else if (length(self@phi) != NROW(self@V)) {
      "The length of @phi must agree with the rows (columns) of @V"
    } else if (length(self@mu) != NROW(self@V)) {
      "The length of @mu must agree with the rows (columns) of @V"
    }
  }
)

#' @title The `rfg_params` Class
#'
#' @description 
#' An S7 class to represent a complete set of parameters for specifying a
#' scalar-valued RFG function. An object of class `<rfg_params>` holds the
#' dimensionality `p` of an RFG function's domain and a `list` of
#' `<rfg_basis_params>`, a collection of parameters necessary to specify each
#' of the basis functions that defines a scalar-valued RFG function.
#' 
#' @seealso [rfg_basis_params()]
#' 
#' @keywords internal
#' @export
rfg_params <- S7::new_class(
  name = "rfg_params",
  properties = list(
    p = prop_int_scalar,
    bases = prop_rfg_bases
  ),
  validator = function(self) {
    for (idx in seq_along(self@bases)) {
      basis_params <- self@bases[[idx]]
      
      if (any(basis_params@phi > self@p)) {
        msg <- sprintf(
          paste(
            "Basis function %i uses a covariate index (%i) in @phi",
            "that will exceed the dimensionality @p of the domain (%i)"  
          ),
          idx, max(basis_params@phi), self@p
        )
        return (msg)
      }
    }
  }
)

#' @title The `rfg_function` Class
#'
#' @description
#' An S7 class to represent a callable RFG function returned by [rfg()]. It
#' inherits from `S7::class_function` and holds the list of `<rfg_params>`
#' objects that specify an RFG function.
#'
#' @keywords internal
#' @export
rfg_function <- S7::new_class(
  name = "rfg_function",
  parent = S7::class_function,
  properties = list(
    .params = prop_rfg_params_list,
    params  = prop_rfg_params_list_read_only
  ),
  constructor = function(..., params) {
    S7::new_object(..., .params = params)
  }
)


