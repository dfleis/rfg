#' @include S7-validators.R
NULL

#----- S7 property factory methods
.make_prop_numeric <- function(validator, ...) {
  S7::new_property(
    class = S7::class_numeric,
    validator = validator, 
    ...
  )
}
.make_prop_list <- function(validator = .make_validator_list(), ...) {
  S7::new_property(
    class = S7::class_list,
    validator = validator,
    ...
  )
}

#----- Specific S7 properties
prop_int_scalar <- .make_prop_numeric(validator_int_scalar)
prop_int_vector <- .make_prop_numeric(validator_int_vector)
prop_num_scalar <- .make_prop_numeric(validator_num_scalar)
prop_num_vector <- .make_prop_numeric(validator_num_vector)
prop_num_sq_matrix <- .make_prop_numeric(validator_num_sq_matrix)
