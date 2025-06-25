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
    .assert_S7_generator_name(Classes[[i]], arg = arg_name)
  })
  
  .validator_list(types = types, ...)
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

# The `rfg_bases` property is just any list of <rfg_basis_params>
validator_rfg_bases <- .make_validator_list_S7(rfg_basis_params)
# TODO The inclusion of `rfg_basis_params` and `rfg_params` below will break
# TODO the package build because this file cannot see the necessary class
# TODO definitions. At the top of this file, I need to do something like 
# TODO    @include S7-classes.R
# TODO However, it's not immediately clear to me whether this is in fact the
# TODO solution because there might be some kind of recursive dependency
# TODO loop since the @include chain looks like
# TODO    S7-utils.R -> S7-validators.R -> S7-properties.R -> S7-classes.R 
# TODO I have not yet tested this, and so I can't say whether it will be a
# TODO problem (the solution would be to simply re-organize the file contents).
# TODO
# TODO This is just a big note to myself regarding something of which I ought
# TODO to stay aware.

# The `rfg_params_list` property is a list of <rfg_params> where each entry of
# the list is an <rfg_params> instance with the same domain dimensionality @p
validator_rfg_params_list_types <- .make_validator_list_S7(rfg_params)
validator_rfg_params_list <- function(value) {
  res <- validator_rfg_params_list_types(value) # Only validates the types
  if (!is.null(res)) {
    return (res)
  } else if (length(unique(sapply(value, S7::prop, "p"))) != 1L) {
    paste(
      "Every <rfg_params> object in the list must",
      "have the same domain dimensionality @p"
    )
  } else {
    NULL
  }
}
