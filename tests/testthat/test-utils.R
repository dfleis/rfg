library(checkmate)

set.seed(1)
p_vals <- c(1, 2, 3, 4, 5)
mats <- lapply(p_vals, generate_orthonormal_matrix)

test_that("generate_orthonormal_matrix produces a square matrix", {
  for (i in seq_along(p_vals)) {
    expect_matrix(
      mats[[i]], mode = "numeric", any.missing = FALSE,
      nrows = p_vals[i], ncols = p_vals[i], 
      info = sprintf("p = %i", p_vals[i])
    )
  }
})

test_that("generated orthonormal matrices are in fact orthonormal", {
  for (i in seq_along(p_vals)) {
    identity_matrix <- diag(1, p_vals[i], p_vals[i])
    expect_equal(
      tcrossprod(mats[[i]]), identity_matrix, 
      info = sprintf("p = %i", p_vals[i])
    )
    
    expect_equal(
      crossprod(mats[[i]]), identity_matrix, 
      info = sprintf("p = %i", p_vals[i])
    )
  }
})

test_that("with_optional_seed makes code reproducible when seed is given", {
  set.seed(42)
  outside <- rnorm(5)
  inside1 <- with_optional_seed(code = rnorm(5), seed = 42)
  inside2 <- with_optional_seed(code = rnorm(5), seed = 42)
  expect_identical(outside, inside1)
  expect_identical(inside1, inside2)
})

test_that("with_optional_seed has no effect when the seed is NULL", {
  set.seed(42)
  inside1 <- with_optional_seed(code = rnorm(5), seed = NULL)
  inside2 <- with_optional_seed(code = rnorm(5), seed = NULL)
  set.seed(42)
  outside1 <- rnorm(5)
  outside2 <- rnorm(5)
  
  expect_false(isTRUE(all.equal(inside1, inside2)))
  expect_identical(inside1, outside1)
  expect_identical(inside2, outside2)
  
  set.seed(42)
  outside1 <- rnorm(5)
  inside <- with_optional_seed(rnorm(5), seed = NULL)
  outside2 <- rnorm(5)
  set.seed(42)
  expect_identical(c(outside1, inside, outside2), rnorm(15))
})

test_that("with_optional_seed has no effect when the seed is missing", {
  set.seed(42)
  inside1 <- with_optional_seed(code = rnorm(5))
  inside2 <- with_optional_seed(code = rnorm(5))
  set.seed(42)
  outside1 <- rnorm(5)
  outside2 <- rnorm(5)
  
  expect_false(isTRUE(all.equal(inside1, inside2)))
  expect_identical(inside1, outside1)
  expect_identical(inside2, outside2)
  
  set.seed(42)
  outside1 <- rnorm(5)
  inside <- with_optional_seed(rnorm(5))
  outside2 <- rnorm(5)
  set.seed(42)
  expect_identical(c(outside1, inside, outside2), rnorm(15))
})

test_that("with_optional_seed resets the seed state after it's finished", {
  set.seed(1)
  outside1 <- rnorm(5)
  inside <- with_optional_seed(rnorm(5), seed = 42)
  outside2 <- rnorm(5)
  
  set.seed(1)
  expect_identical(c(outside1, outside2), rnorm(10))
})



