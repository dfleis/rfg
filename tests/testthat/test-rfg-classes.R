library(checkmate)

#----- rfg_basis_params class generator
test_that("rfg_basis_params constructor works with valid inputs", {
  expect_s7_class(
    rfg_basis_params(a = 1, phi = 1, mu = 1, V = matrix(1)), 
    rfg_basis_params
  )
  expect_s7_class(
    rfg_basis_params(a = 1, phi = 1:3, mu = 1:3, V = diag(3)),
    rfg_basis_params
  )
})

test_that("rfg_basis_params validator detects invalid combinations of inputs", {
  expect_error(
    rfg_basis_params(a = 1, phi = 1:3, mu = 1:2, V = diag(2)),
    "The length of @phi must be equal to the length of @mu"
  )
  expect_error(
    rfg_basis_params(a = 1, phi = 1:3, mu = 1:2, V = diag(3)),
    "The length of @phi must be equal to the length of @mu"
  )
  expect_error(
    rfg_basis_params(a = 1, phi = 1:3, mu = 1:3, V = diag(2)),
    "The length of @phi must agree with the rows \\(columns\\) of @V"
  )
  expect_error(
    rfg_basis_params(a = 1, phi = 1:2, mu = 1:3, V = diag(3)),
    "The length of @phi must be equal to the length of @mu"
  )
  expect_error(
    rfg_basis_params(a = 1, phi = 1:2, mu = 1:3, V = diag(2)),
    "The length of @phi must be equal to the length of @mu"
  )
})

#----- prop_rfg_bases
test_that("prop_rfg_bases is an S7 property with a legitimate validator", {
  expect_class(prop_rfg_bases, "S7_property")
  expect_function(prop_rfg_bases$validator, args = "value", 
                  nargs = 1, null.ok = FALSE)
})

#----- rfg_params
test_that("rfg_params works with valid inputs", {
  b1 <- rfg_basis_params(a = -0.5, phi = 1, mu = 0.5, V = diag(1))
  b2 <- rfg_basis_params(a = 0.9, phi = 2:3, mu = c(0.5, -0.7), V = diag(2))

  expect_s7_class(rfg_params(p = 1, bases = list(b1)), class = rfg_params)
  expect_s7_class(rfg_params(p = 3, bases = list(b1, b2)), class = rfg_params)
  
  r <- rfg_params(p = 3, bases = list(b1, b2))
  expect_integerish(r@p, len = 1, any.missing = FALSE)
  expect_list(r@bases, len = 2, types = classname(b1), any.missing = FALSE)
  
  expect_lte(max(sapply(r@bases, function(x) max(x@phi))), r@p, 
             label = "Largest permuted index @phi")
})

test_that("rfg_params validator detects invalid combinations of inputs", {
  b1 <- rfg_basis_params(a = -0.5, phi = 1, mu = 0.5, V = diag(1))
  b2 <- rfg_basis_params(a = 0.9, phi = 2:3, mu = c(0.5, -0.7), V = diag(2))
  
  expect_error(rfg_params(p = 1), "@bases Must have length >= 1, but has length 0")
  expect_error(rfg_params(bases = list(b1)), "@p Must have length 1, but has length 0")
  expect_error(
    rfg_params(p = 1, bases = list(b2)),
    paste(
      "Basis function 1 uses a covariate index \\(3\\) in @phi",
      "that will exceed the dimensionality @p of the domain \\(1\\)"
    )
  )
  
  r <- rfg_params(p = 3, bases = list(b1, b2))
  expect_error(
    r@p <- 1,
    paste(
      "Basis function 2 uses a covariate index \\(3\\) in @phi",
      "that will exceed the dimensionality @p of the domain \\(1\\)"
    )
  )
  expect_error(
    r@bases[[1]]@phi <- 4,
    paste(
      "Basis function 1 uses a covariate index \\(4\\) in @phi",
      "that will exceed the dimensionality @p of the domain \\(3\\)"
    )
  )
})

#----- validator_rfg_params_list
test_that("validator_rfg_params_list identifies lists of rfg_params", {
  b1 <- rfg_basis_params(a = 1, phi = 1, mu = 1, V = diag(1))
  b2 <- rfg_basis_params(a = 1, phi = 2, mu = 1, V = diag(1))
  b3 <- rfg_basis_params(a = 1, phi = 1:3, mu = c(1, 1, 1), V = diag(3))
  
  r1 <- rfg_params(p = 2, bases = list(b1, b2))
  r2 <- rfg_params(p = 3, bases = list(b1, b2))
  r3 <- rfg_params(p = 3, bases = list(b1, b2))
  
  expect_match(
    validator_rfg_params_list(b1),
    "Must be of type 'list', not 'rfg::rfg_basis_params/S7_object'"
  )
  expect_match(
    validator_rfg_params_list(list(b1)),
    paste(
      "May only contain the following types: \\{rfg::rfg_params\\}, but",
      "element 1 has type 'rfg::rfg_basis_params,S7_object'"
    )
  )
  
  expect_match(
    validator_rfg_params_list(r1),
    "Must be of type 'list', not 'rfg::rfg_params/S7_object'"
  )
  
  expect_null(validator_rfg_params_list(list(r1)))
  expect_null(validator_rfg_params_list(list(r2, r3)))
})

test_that("validator_rfg_params_list correctly rejects different domain dimensionalities", {
  b1 <- rfg_basis_params(a = 1, phi = 1, mu = 1, V = diag(1))
  b2 <- rfg_basis_params(a = 1, phi = 2, mu = 1, V = diag(1))

  r1 <- rfg_params(p = 2, bases = list(b1, b2))
  r2 <- rfg_params(p = 3, bases = list(b1, b2))

  expect_match(
    validator_rfg_params_list(list(r1, r2)),
    paste(
      "Every <rfg_params> object in the list",
      "must have the same domain dimensionality @p"
    )
  )
})

#----- rfg_function
test_that("rfg_function produces a legitimate function with read-only parameters", {
  b1 <- rfg_basis_params(a = 1, phi = 1, mu = 1, V = diag(1))
  b2 <- rfg_basis_params(a = 1, phi = 2, mu = 1, V = diag(1))
  b3 <- rfg_basis_params(a = 1, phi = 1:3, mu = c(1, 1, 1), V = diag(3))
  
  r1 <- rfg_params(p = 3, bases = list(b1, b2))
  r2 <- rfg_params(p = 3, bases = list(b3))

  expect_function(rfg_function(function(...){NULL}, params = list(r1, r2)))
  
  f <- rfg_function(function(...){NULL}, params = list(r1, r2))

  expect_error(
    f@params[[1]] <- NULL, 
    "Can't set read-only property <rfg::rfg_function>@params"
  )
})
