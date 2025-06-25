#' @include classes.R
#' @include S7-utils.R
NULL

#----- Print utilities
#' @keywords internal
#' @noRd
cap_str <- function(x, give.attr = FALSE, ...) {
  utils::capture.output(utils::str(x, give.attr = give.attr, ...))
}

#' @keywords internal
#' @noRd
fmt_classname <- function(x, ...) {
  sprintf("<%s>", classname(x))
}

#----- Print methods
#' @export
S7::method(print, rfg_basis_params) <- function(x, detailed = TRUE, ...) {
  if (!isTRUE(detailed)) {
    class_info <- fmt_classname(x)
    
    str_a <- sprintf("@a %6.3f", x@a)
    str_phi <- sprintf("@phi %s", cap_str(x@phi, give.head = F, vec.len = 2)[1])
    
    str_mu <- sprintf("@mu%s", sub(" NULL ...", "", cap_str(x@mu, vec.len = 0)[1]))
    if (length(x@mu) == 1L) {
      str_mu <- paste(str_mu, "[1:1]")
    }
    str_V <- sprintf("@V%s", sub(" NULL ...", "", cap_str(x@V, vec.len = 0)[1]))
    
    cat(class_info, paste(str_a, str_mu, str_V, str_phi, sep = ", "), "\n")
    return(invisible(x))
  } else {
    NextMethod()
  }
}

#' @export
S7::method(print, rfg_params) <- function(x, detailed = FALSE, ...) {
  cat(classname(x), "\n")
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
  # cat(fmt_classname(x), "\n")
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
