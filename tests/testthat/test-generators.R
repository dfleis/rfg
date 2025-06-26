library(checkmate)
library(utils)

args_default <- list(
  num.funs = 20, a.min = -1, a.max = 1, 
  lambda = 2, d.min = 0.1, d.max = 2
)
.make_args <- function(..., args = args_default) {
  modifyList(args_default, list(...))
}

test_that("generate_scalar_rfg_params returns rfg_params", {
  set.seed(1)
  scalar_params1 <- do.call(generate_scalar_rfg_params, .make_args(p = 1))
  expect_s7_class(scalar_params1, rfg_params)
  
  set.seed(1)
  scalar_params2 <- do.call(generate_scalar_rfg_params, .make_args(p = 2))
  expect_s7_class(scalar_params2, rfg_params)
})

test_that("generate_rfg_params returns a list of rfg_params", {
  scalar_params <- do.call(generate_scalar_rfg_params, .make_args(p = 1))
  
  set.seed(1)
  multi_params1 <- do.call(generate_rfg_params, .make_args(p = 1, q = 1))
  expect_list(
    multi_params1, any.missing = FALSE,
    len = 1, types = classname(scalar_params)
  )
  
  set.seed(1)
  multi_params2 <- do.call(generate_rfg_params, .make_args(p = 7, q = 10))
  expect_list(
    multi_params2, any.missing = FALSE,
    len = 10, types = classname(scalar_params)
  )
  
  domain_dims <- sapply(multi_params2, function(params) params@p)
  expect_equal(unique(domain_dims), 7)
})

test_that("generate_scalar_rfg_params is a special case of generate_rfg_params", {
  set.seed(1)
  scalar_params <- do.call(generate_scalar_rfg_params, .make_args(p = 1))
  set.seed(1)
  multi_params <- do.call(generate_rfg_params, .make_args(p = 1, q = 1))
  
  expect_identical(scalar_params, multi_params[[1]])
})


