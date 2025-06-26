library(checkmate)

Car_S7 <- S7::new_class("Car")
Car_S4 <- setClass("Car_S4", slots = list(make = "character"))
Car_S3 <- list(); class(Car_S3) <- "Car_S3"
Car <- list()
my_car_S7 <- Car_S7()
my_car_S4 <- new("Car_S4")

test_that("is_S7_Class is true for only S7 class generators", {
  expect_true(is_S7_Class(Car_S7)) # TRUE
  expect_false(is_S7_Class(Car_S4))
  expect_false(is_S7_Class(Car_S3))
  expect_false(is_S7_Class(Car))
  expect_false(is_S7_Class(my_car_S7))
  expect_false(is_S7_Class(my_car_S4))
})

test_that("is_S7_instance is true for only instances of S7 classes", {
  expect_false(is_S7_instance(Car_S7))
  expect_false(is_S7_instance(Car_S4))
  expect_false(is_S7_instance(Car_S3))
  expect_false(is_S7_instance(Car))
  expect_true(is_S7_instance(my_car_S7)) # TRUE
  expect_false(is_S7_instance(my_car_S4))
})

test_that("obj_type returns whether an object is S7, S4, S3, or base", {
  expect_identical(obj_type(Car_S7), "S7")
  expect_identical(obj_type(Car_S4), "S4")
  expect_identical(obj_type(Car_S3), "S3")
  expect_identical(obj_type(Car), "base")
  expect_identical(obj_type(quote(expr=)), "missing") # missing argument
  expect_identical(obj_type(my_car_S7), "S7")
  expect_identical(obj_type(my_car_S4), "S4")
})

test_that("obj_desc properly formats class labels", {
  expect_identical(obj_desc(Car_S7), "<S7_class>")
  expect_identical(obj_desc(Car_S4), "S4<classGeneratorFunction>")
  expect_identical(obj_desc(Car_S3), "S3<Car_S3>")
  expect_identical(obj_desc(Car), "<list>")
  expect_identical(obj_desc(quote(expr=)), "MISSING") # missing argument
  expect_identical(obj_desc(my_car_S7), "<rfg::Car>")
  expect_identical(obj_desc(my_car_S4), "S4<Car_S4>")
})

test_that("classname returns the name of the S7 class generator", {
  expect_identical(classname(Car_S7), "rfg::Car")
  expect_identical(classname(my_car_S7), "rfg::Car")
})

test_that("assert_S7_generator_name returns the name of only S7 class generators", {
  expect_identical(assert_S7_generator_name(Car_S7), "rfg::Car")
  expect_error(assert_S7_generator_name(my_car_S7))  
})
