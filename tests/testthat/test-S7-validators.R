####################################################################################################
# test-S7-validators.R
#
# .build_prop_validator
#
# .make_prop_validator_num
# .make_prop_validator_int
# .make_prop_validator_list
# .make_prop_validator_list_S7
# 
# validator_int_scalar
# validator_int_vector
# validator_num_scalar
# validator_num_vector
# validator_num_matrix
# validator_num_sq_matrix
#
####################################################################################################
library(checkmate)

#----- .build_prop_validator factory
test_that(".build_prop_validator returns a function", {
  check_fn <- checkmate::check_true
  expect_function(.build_prop_validator(check_fn))
  expect_function(.build_prop_validator(check_fn), args = "value", nargs = 1, 
                  info = "Property validators must only have a single argument, `value`.")
})

test_that(".build_prop_validator returns a function that returns NULL if true", {
  check_fn <- checkmate::check_true
  validator_fn <- .build_prop_validator(check_fn)
  expect_null(validator_fn(TRUE))
})

test_that(".build_prop_validator returns a function that returns a string if false", {
  check_fn <- checkmate::check_true
  validator_fn <- .build_prop_validator(check_fn)
  expect_string(validator_fn(FALSE))
})

test_that(".build_prop_validator correctly passes arguments to the check function", {
  check_fn <- checkmate::check_choice
  validator_fn <- .build_prop_validator(check_fn, choices = c("a", "b"))
  expect_null(validator_fn("a"))
  expect_null(validator_fn("b"))
  expect_string(validator_fn("c"))
})

#----- .make_prop_validator_num factory
test_that(".make_prop_validator_num works correctly", {
  validator_fn <- .make_prop_validator_num()
  expect_null(validator_fn(1.0))
  expect_null(validator_fn(1L))
  expect_null(validator_fn(c(0.5, 1L, 1.5)))
  
  expect_string(validator_fn(integer(0)))
  expect_string(validator_fn(numeric(0)))
  expect_string(validator_fn(NA))
  expect_string(validator_fn(NaN))
  expect_string(validator_fn(c(1, NA)))
})

#----- .make_prop_validator_int factory
test_that(".make_prop_validator_int works correctly", {
  validator_fn <- .make_prop_validator_int()
  expect_null(validator_fn(1L))  
  expect_null(validator_fn(1.0)) 
  expect_null(validator_fn(c(1.0, 1L)))
  
  expect_string(validator_fn(integer(0)))
  expect_string(validator_fn(numeric(0)))
  expect_string(validator_fn(NA))
  expect_string(validator_fn(NaN))
  expect_string(validator_fn(c(1, NA)))
  
  tol <- sqrt(.Machine$double.eps)
  expect_null(validator_fn(1.0 - tol))  
  expect_null(validator_fn(1.0 + tol))  
  expect_string(validator_fn(1.0 - tol * 1.01))
  expect_string(validator_fn(1.0 + tol * 1.01)) 
})
  
#----- .make_prop_validator_list factory
test_that(".make_prop_validator_list works correctly", {
  vals <- list(3.14, "a", 2.71, "b")
  types <- unique(sapply(vals, class))
  
  validator_fn <- .make_prop_validator_list(types = types)
  
  expect_null(validator_fn(vals))
  expect_null(validator_fn(list(1.0)))
  expect_null(validator_fn(list(1.0, "c")))
  
  expect_string(validator_fn(character(0)))
  expect_string(validator_fn(numeric(0)))
  expect_string(validator_fn(1.0))
  expect_string(validator_fn(list(1.0, "c", TRUE)))
  
  expect_string(validator_fn(list()))
  expect_string(validator_fn(list(list(vals))))
})

#----- .make_prop_validator_list_S7
test_that(".make_prop_validator_list_S7 works correctly", {
  Cat <- S7::new_class("Cat")
  Dog <- S7::new_class("Dog")
  
  my_cat <- Cat()
  my_dog <- Dog()
  
  expect_function(.make_prop_validator_list_S7(Cat))
  expect_function(.make_prop_validator_list_S7(list(Cat, Dog)))
  
  expect_error(.make_prop_validator_list_S7(my_cat), 
               "must be an <S7_class>, not an <S7_object>")
  expect_error(.make_prop_validator_list_S7(list(Cat, my_dog)), 
               "must be an <S7_class>, not an <S7_object>")
  expect_error(.make_prop_validator_list_S7(list(my_cat, Dog)), 
               "must be an <S7_class>, not an <S7_object>")
  
  expect_error(.make_prop_validator_list_S7(list(Cat, "a", Dog)),
               "must be an <S7_class>, not a <character>")
  expect_error(.make_prop_validator_list_S7(list(Cat, NA, Dog)),
               "must be an <S7_class>, not a <logical>")
  expect_error(.make_prop_validator_list_S7(list(Cat, NaN, Dog)),
               "must be an <S7_class>, not a <double>")
  expect_error(.make_prop_validator_list_S7(list(Cat, quote(expr=), Dog)),
               "must be an <S7_class>, not a MISSING")
  expect_error(.make_prop_validator_list_S7(list(Cat, NULL, Dog)),
               "must be an <S7_class>, not a <NULL>")
  
  Bootleg_Cat1 <- "Moo"; class(Bootleg_Cat1) <- "Cat"
  Bootleg_Cat2 <- "Moo"; class(Bootleg_Cat2) <- class(Cat)
  expect_error(.make_prop_validator_list_S7(Bootleg_Cat1))
  expect_error(.make_prop_validator_list_S7(Bootleg_Cat2))
  
  
})

test_that(".make_prop_validator_list_S7 produces a correct validator with rfg_basis_params", {
  validator_fn <- .make_prop_validator_list_S7(rfg_basis_params)
  
  valid_basis_1 <- rfg_basis_params(a = 1, phi = 2, mu = 2.5, V = matrix(4))
  valid_basis_2 <- rfg_basis_params(a = -1, phi = 5, mu = -6, V = matrix(3.5))
  invalid_basis <- list(a = 1, phi = 2, mu = 2.5, V = matrix(4))
  
  expect_null(validator_fn(list(valid_basis_1)))
  expect_null(validator_fn(list(valid_basis_1, valid_basis_2)))

  expect_match(
    validator_fn(valid_basis_1), 
    "Must be of type 'list', not 'rfg::rfg_basis_params/S7_object'"
  )
  expect_match(
    validator_fn(invalid_basis),
    "May only contain the following types: \\{rfg::rfg_basis_params\\}, but element 1 has type 'numeric'"
  )
  expect_match(
    validator_fn(list(invalid_basis)),
    "May only contain the following types: \\{rfg::rfg_basis_params\\}, but element 1 has type 'list'"
  )
  expect_match(
    validator_fn(list(valid_basis_1, invalid_basis)),
    "May only contain the following types: \\{rfg::rfg_basis_params\\}, but element 2 has type 'list'"
  )
})

#----- validator_num_matrix
test_that("validator_num_matrix passes and fails correctly", {
  expect_null(validator_num_matrix(matrix(1:6, nrow = 2, ncol = 3)))
  expect_null(validator_num_matrix(matrix(1)))
  expect_match(validator_num_matrix(1:6), "Must be of type 'matrix'")
  expect_match(validator_num_matrix(matrix(c("a", "b"), 1, 2)), "Must store numerics")
})

#----- validator_num_sq_matrix
test_that("validator_num_sq_matrix passes and fails correctly", {
  expect_null(validator_num_sq_matrix(matrix(1)))
  expect_null(validator_num_sq_matrix(matrix(1:4, nrow = 2, ncol = 2)))
  expect_null(validator_num_sq_matrix(matrix(1:9, nrow = 3, ncol = 3)))
  
  expect_match(validator_num_sq_matrix(matrix()), 
               "Contains missing values \\(row 1, col 1\\)")
  expect_match(validator_num_sq_matrix(matrix(NA)), 
               "Contains missing values \\(row 1, col 1\\)")
  expect_match(validator_num_sq_matrix(matrix(1:6, nrow = 2, ncol = 3)),
               "Must be a square matrix")
  expect_match(validator_num_sq_matrix(1:4), "Must be of type 'matrix'")
})