#' @include S7-utils.R
NULL

#----- Factory methods for S7 validator functions
.build_validator <- function(check_fn, ...) {
  args <- list(...)
  function(value) {
    args <- utils::modifyList(args, list(x = value))
    checked_value <- do.call(check_fn, args)
    if (!isTRUE(checked_value)) return (checked_value)
    return (NULL)
  }
}

.make_validator_num <- function(any.missing = FALSE, min.len = 1, ...) {
  .build_validator(
    check_fn = checkmate::check_atomic_vector, 
    any.missing = any.missing, 
    min.len = min.len, 
    ...
  )
}
.make_validator_int <- function(any.missing = FALSE, min.len = 1, ...) {
  .build_validator(
    check_fn = checkmate::check_integerish, 
    any.missing = any.missing, 
    min.len = min.len, 
    ...
  )
}
.make_validator_list <- function(any.missing = FALSE, min.len = 1, ...) {
  .build_validator(
    check_fn = checkmate::check_list, 
    any.missing = any.missing, 
    min.len = min.len, 
    ...
  )
}
.make_validator_list_S7 <- function(Classes, ...) {
  if (!is.list(Classes)) Classes <- list(Classes)
  list_name <- deparse(substitute(Classes))
  
  types <- sapply(seq_along(Classes), function(i) {
    arg_name <- sprintf("%s[[%i]]", list_name, i)
    assert_S7_generator_name(Classes[[i]], arg = arg_name)
  })
  
  .make_validator_list(types = types, ...)
}

#----- Specific validator functions
validator_int_scalar <- .make_validator_int(len = 1)
validator_int_vector <- .make_validator_int()
validator_num_scalar <- .make_validator_num(len = 1)
validator_num_vector <- .make_validator_num()

validator_num_matrix <- .build_validator(
  checkmate::check_matrix, mode = "numeric", 
  any.missing = FALSE, min.rows = 1, min.cols = 1
)
validator_num_sq_matrix <- .build_validator(
  function(x, ...) {
    res <- checkmate::check_matrix(x = x, ...)
    if (!isTRUE(res)) {
      return (res)
    } else if (NROW(x) != NCOL(x)) {
      return ("Must be a square matrix")
    } else {
      TRUE
    }
  },
  mode = "numeric", any.missing = FALSE, min.rows = 1, min.cols = 1
)
