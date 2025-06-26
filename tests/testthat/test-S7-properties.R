library(checkmate)

test_that(".make_prop_numeric", {
  example_prop_num <- .make_prop_numeric(validator = function(value) TRUE)
  expect_class(example_prop_num, "S7_property")
  expect_class(example_prop_num$class, "S7_union")
  expect_list(example_prop_num$class$classes, types = "S7_base_class")
  expect_set_equal(
    sapply(example_prop_num$class$classes, function(x) x$class),
    c("integer", "double")
  )
})
