#' @include classes.R
NULL

#' @title Generate Parameters for a Scalar-Valued RFG Function
#'
#' @description
#' Generates a complete set of parameters for a scalar-valued RFG function.
#'
#' @param p The dimensionality of the RFG function's domain.
#' @param num.funs The number of Gaussian basis functions to sum. Corresponds
#'    to the parameter \eqn{L} in the original paper.
#' @param a.min,a.max The lower and upper bounds for the uniform distribution
#'    from which the linear coefficients `a` are drawn.
#' @param lambda The rate parameter for the exponential distribution used to
#'    determine the latent dimensionality of each basis function.
#' @param d.min,d.max The lower and upper bounds for the uniform distribution
#'    from which the singular values of the covariance matrices `V` are drawn.
#'
#' @return An object of class `<rfg_params>`.
#' 
#' @seealso [rfg()]
#' @template friedman-reference
#' 
#' @keywords internal
generate_scalar_rfg_params <- function(p, num.funs, a.min, a.max, 
                                       lambda, d.min, d.max) {
  bases <- replicate(num.funs, {
    r <- stats::rexp(1, 1.0/lambda)
    p_latent <- min(floor(1.5 + r), p)
    U <- generate_orthonormal_matrix(p_latent)
    d <- stats::runif(p_latent, d.min, d.max)^2
    rfg_basis_params(
      a   = stats::runif(1, a.min, a.max),
      phi = sample(p, size = p_latent, replace = FALSE),
      mu  = stats::rnorm(p_latent),
      V   = U %*% diag(d, nrow = p_latent, ncol = p_latent) %*% t(U)
    )
  }, simplify = FALSE)
  
  rfg_params(p = p, bases = bases)
}

#' @title Generate Parameters for an RFG Function
#'
#' @details
#' Generates a collection of complete parameter sets for a (potentially
#' vector-valued) RFG function.
#'
#' @details 
#' We define a vector-valued RFG function as a collection of 
#' independently-generated scalar-valued RFG functions. Specifically, if 
#' \eqn{F^*(x) \in \mathbb R^q} is the \eqn{q}-dimensional output of a
#' vector-valued RFG function \eqn{F^*:\mathbb R^p\to\mathbb R^q}, then 
#' \deqn{F^*(x) = (F_1^*(x),\ldots,F_q^*(x))}
#' where each \eqn{F_j^*(x) \in \mathbb R} is the scalar output of a
#' scalar-valued RFG function \eqn{F_j^*:\mathbb R^p\to\mathbb R}.
#'
#' @param q The output dimensionality.
#' @inheritParams generate_scalar_rfg_params
#'
#' @return An `list` of `<rfg_params>`, each specifying the parameters for a
#'    single scalar-valued RFG function.
#' 
#' @seealso [rfg()]
#' @template friedman-reference
#' 
#' @export
generate_rfg_params <- function(
    p, 
    q = 1L,
    num.funs = 20L,
    a.min    = -1,
    a.max    = 1,
    lambda   = 2,
    d.min    = 0.1,
    d.max    = 2) {
  all_params <- lapply(
    seq_len(q), 
    function(i) {
      generate_scalar_rfg_params(
        p = p,
        num.funs = num.funs, 
        a.min = a.min, 
        a.max = a.max, 
        lambda = lambda, 
        d.min = d.min, 
        d.max = d.max
      )
    }
  )
  all_params
}
