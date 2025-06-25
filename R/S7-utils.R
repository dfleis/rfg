#' @keywords internal
#' @noRd
is_S7_Class <- function(Class) {
  # Test for class generators, adapted from S7:::is_S7_type
  S7::S7_inherits(Class) && (typeof(Class) == typeof(S7::new_class(".")))
}

#' @keywords internal
#' @noRd
is_S7_instance <- function(x) {
  # Test for class instances, adapted from S7:::is_S7_type
  S7::S7_inherits(x) && (typeof(x) == typeof(S7::S7_object()))
}

#' @keywords internal
#' @noRd
obj_type <- function (x) {
  # Taken from S7:::obj_type
  if (identical(x, quote(expr = ))) {
    "missing"
  } else if (inherits(x, "S7_object")) {
    "S7"
  } else if (isS4(x)) {
    "S4"
  } else if (is.object(x)) {
    "S3"
  } else {
    "base"
  }
}

#' @keywords internal
#' @noRd
obj_desc <- function (x) {
  # Taken from S7:::obj_desc
  switch(
    obj_type(x),
    missing = "MISSING",
    base = paste0("<", typeof(x), ">"),
    S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
    S4 = paste0("S4<", class(x), ">"),
    S7 = paste0("<", class(x)[[1]], ">")
  )
}

#' @keywords internal
#' @noRd
classname <- S7::new_generic(".classname", "x")

#' @keywords internal
#' @noRd
S7::method(classname, S7::S7_object) <- function(x, ...) {
  if (is_S7_instance(x)) {
    x <- S7::S7_class(x)
  }
  pkg <- x@package # S7::prop(x, "package")
  nm <- x@name     # S7::prop(x, "name")
  sprintf("%s%s", toString(sprintf("%s::", pkg)), nm)
}

#' @keywords internal
#' @noRd
assert_S7_generator_name <- function(Class, arg = deparse(substitute(Class))) {
  # Gets the class name for instances of an S7 class generator `Class`
  # Adapted from S7:::S7_class_name, S7::S7_inherits, S7::check_is_S7
  if (!isTRUE(is_S7_Class(Class))) {
    msg <- sprintf(
      "Object %s must be an <S7_class>, not %s", arg,
      if (is_S7_instance(Class)) "an <S7_object>"
      else paste("a", obj_desc(Class))
    )
    stop(msg, call. = FALSE)
  }
  classname(Class)
}
