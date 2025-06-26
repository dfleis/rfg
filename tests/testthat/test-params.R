library(checkmate)

test_that("params() correctly extracts the parameter list", {
  rfg_fun <- rfg(p = 3, q = 2, seed = 1)
  extracted_params <- params(rfg_fun)
  expect_identical(extracted_params, rfg_fun@params)
  expect_list(extracted_params, types = classname(rfg_params), len = 2)
})