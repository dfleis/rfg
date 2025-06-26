library(checkmate)

test_that("rfg() creates a valid, reproducible function", {
  rfg_fn_1 <- rfg(p = 5, q = 2, seed = 123)
  
  expect_true(is.function(rfg_fn_1))
  expect_s7_class(rfg_fn_1, rfg_function)
  
  expect_list(rfg_fn_1@params, types = classname(rfg_params), len = 2)
  
  expect_equal(rfg_fn_1@params[[1]]@p, 5)
  expect_equal(rfg_fn_1@params[[2]]@p, 5)
  
  rfg_fn_2 <- rfg(p = 5, q = 2, seed = 123)
  expect_identical(rfg_fn_1@params, rfg_fn_2@params)
})

test_that("rfg() from p = rfg_params creates a RFG", {
  set.seed(1)
  my_params <- generate_rfg_params(p = 5, q = 2)
  rfg_fn <- rfg(my_params) 
  
  expect_s7_class(rfg_fn, rfg_function)
  expect_identical(rfg_fn@params, my_params)
  
  # Check that other arguments are correctly ignored
  rfg_fn_ignored <- rfg(my_params, q = 10, seed = 999)
  expect_identical(rfg_fn@params, rfg_fn_ignored@params)
})


#---- Testing the generated  function values

test_that("generated rfg_function correctly handles inputs and dimensions", {
  set.seed(1)
  n <- 10
  p <- 5
  q <- 2
  
  X <- matrix(rnorm(n * p), nrow = n, ncol = p, byrow = T)
  rfg_fn_p5_q2 <- rfg(p = p, q = q)

  Y <- rfg_fn_p5_q2(X)
  expect_matrix(Y, nrows = n, ncols = q, mode = "numeric", any.missing = F)

  set.seed(1)
  x1 <- matrix(rnorm(1 * p), nrow = 1, byrow = T)
  y1 <- rfg_fn_p5_q2(x1)
  expect_identical(y1, Y[1,])
  

  X_bad <- matrix(runif(n * 4), nrow = n, ncol = 4, byrow = T)
  expect_error(
    rfg_fn_p5_q2(X_bad),
    "RFG function inputs must have 5 columns"
  )
  
  x_vec <- rnorm(p)
  expect_error(
    rfg_fn_p5_q2(x_vec),
    "RFG function inputs must have 5 columns"
  )
})

test_that("generated rfg_function is numerically correct for a simple case", {
  # Manually create a dead-simple, non-random basis function
  # phi=1 means it only uses the first column of x
  # mu=0, V=diag(1) means the exponent is just -0.5 * x[,1]^2
  # a=2 means the whole thing is 2 * exp(-0.5 * x[,1]^2)
  simple_basis <- rfg_basis_params(a = 2, phi = 1, mu = 0, V = matrix(1))
  simple_params <- rfg_params(p = 1, bases = list(simple_basis))

  simple_rfg <- rfg(list(simple_params))
  
  set.seed(1)
  n <- 10
  X <- matrix(rnorm(n), nrow = n)
  Y <- simple_rfg(X)

  Y_expected <- 2 * exp(-0.5 * X^2)
  expect_equal(Y, Y_expected)
})