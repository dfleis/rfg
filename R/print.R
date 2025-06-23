#' @include classes.R
NULL

#' @keywords internal
#' @noRd
.format_classname <- S7::new_generic("format_classname", "x")

#' @keywords internal
#' @noRd
S7::method(.format_classname, S7::S7_object) <- function(x, ...) {
  class_obj <- S7::S7_class(x)
  pkg <- class_obj@package # S7::prop(class_obj, "package")
  nm <- class_obj@name # S7::prop(class_obj, "name")
  sprintf("<%s%s>", toString(sprintf("%s::", pkg)), nm)
}

#' @export
S7::method(print, rfg_basis_params) <- function(x, detailed = TRUE, ...) {
  if (!isTRUE(detailed)) {
    class_info <- .format_classname(x)
    
    str_a <- sprintf("@a %6.3f", x@a)
    str_phi <- sprintf("@phi %s", capture.output(str(x@phi, give.head = F, vec.len = 2))[1])
    
    str_mu <- sprintf("@mu%s", sub(" NULL ...", "", capture.output(str(x@mu, vec.len = 0))[1]))
    if (length(x@mu) == 1L) {
      str_mu <- paste(str_mu, "[1]")
    }
    str_V <- sprintf("@V%s", sub(" NULL ...", "", capture.output(str(x@V, vec.len = 0))[1]))
    
    cat(class_info, paste(str_a, str_phi, str_mu, str_V, sep = ", "), "\n")
    return(invisible(x))
  } else {
    NextMethod()
  }
}

#' @export
S7::method(print, rfg_params) <- function(x, detailed = FALSE, ...) {
  cat(sprintf("%s\n", .format_classname(x)))
  cat(" @ p    :")
  cat(capture.output(str(x@p, give.attr = FALSE))[1], "\n")
  cat(" @ bases:")
  cat(capture.output(str(x@bases, max.level = 0, give.attr = FALSE))[1], "\n")
  
  for (i in seq_along(x@bases)) {
    print_str <- capture.output(print(x@bases[[i]], detailed = detailed))
    cat(sprintf(" .. %s\n", print_str))
  }
  
  invisible(x)
}

#' @export
S7::method(print, rfg_function) <- function(x, detailed = FALSE, ...) {
  # TODO 
  # TODO
  # TODO
  #
  # param_list <- x@params
  # 
  # q_dim <- length(param_list)
  # p_dim <- if (q_dim > 0L) param_list[[1]]@p else 0L
  # 
  # cat(sprintf("Generated RFG map from R^%i to R^%i\n", p_dim, q_dim))
  # cat(sprintf("%s\n", .format_classname(x)))
  # 
  # if (q_dim > 0L) {
  #   cat(sprintf(" @ params:%s of %i\n", class(param_list)[1], q_dim))
  #   for (i in seq_len(q_dim)) {
  #     num_bases <- length(param_list[[i]]@bases)
  #     p_dim_i <- param_list[[i]]@p
  #     cat(
  #       sprintf(
  #         " .. [[%i]] Scalar-valued RFG function R^%i -> R (%i basis functions)\n",
  #         i, p_dim_i, num_bases
  #       )
  #     )
  #   }
  # }
  # invisible(x)
  NextMethod()
}
