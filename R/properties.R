#' @keywords internal
#' @noRd
.always_true <- function(x) TRUE

#' @keywords internal
#' @noRd
.make_prop_numeric <- function(name = NULL, .is_valid_checker = .always_true, ...) {
  S7::new_property(
    name = name,
    class = S7::class_numeric,
    validator = function(value) {
      is_valid <- isTRUE(.is_valid_checker(value))
      if (!is_valid) {
        paste("Underlying data must be a", name)
      }
    },
    ...
  )
}

#' @keywords internal
#' @noRd
prop_numeric_scalar <- .make_prop_numeric("scalar", function(x) length(x) == 1L)

#' @keywords internal
#' @noRd
prop_numeric_vector <- .make_prop_numeric("numeric", function(x) length(x) > 0L)

#' @keywords internal
#' @noRd
prop_numeric_matrix <- .make_prop_numeric("matrix", is.matrix)

#' @keywords internal
#' @noRd
prop_dim_domain <- S7::new_property(
  class = S7::class_numeric,
  validator = function(value) {
    if (length(value) != 1L) {
      paste(
        "Must be an integer value representing the",
        "dimensionality of the RFG function domain" 
      )
    }
  }
)

#' @keywords internal
#' @noRd
prop_bases <- S7::new_property(
  name = "bases",
  class = S7::class_list,
  validator = function(value) {
    if (length(value) == 0L) {
      "Must be a nonempty list of <rfg_basis_params> objects"
    } else if (!all(sapply(value, S7::S7_inherits, class = rfg_basis_params))) {
      "Must be a list of only <rfg_basis_params> objects"
    }
  }
)

#' @keywords internal
#' @noRd
.make_prop_rfg_params_list <- function(...) {
  S7::new_property(
    class = S7::class_list,
    validator = function(value) {
      if (length(value) == 0L) {
        "Must be a nonempty list of <rfg_params> objects"
      } else if (!all(sapply(value, S7::S7_inherits, class = rfg_params))) {
        "Must be a list of only <rfg_params> objects"
      } else if (length(unique(sapply(value, S7::prop, "p"))) != 1L) {
        paste(
          "Every <rfg_params> object in the list must",
          "have the same domain dimensionality @p"
        )
      }
    },
    ...
  )
}
