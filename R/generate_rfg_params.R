#' @title Generate Random Function Generator Parameters
#'
#' @description Generates parameters for the random function generator (RFG) as described in 
#'   Friedman (2001).
#'
#' @param p Input dimensionality, number of input variables.
#' @param num.funs Number of basis functions to use for the random function generation. 
#'    Corresponds to the parameter \eqn{L} described in [rfg()]. Default `num.funs = 20`.
#' @param a.min Lower limit for the uniform distribution of linear coefficients \eqn{a_\ell}.
#'    Default `a.min = -1`.
#' @param a.max Upper limit for the uniform distribution of linear coefficients \eqn{a_\ell}.
#'    Default `a.max = 1`.
#' @param lambda Rate parameter for the exponential random variable \eqn{r\sim \text{Exp}(\lambda)}
#'    described in [rfg()]. Default `lambda = 2`.
#' @param d.min Lower limit for singular values \eqn{\sqrt{d_j}} used to generate the 
#'    covariance matrix \eqn{V_\ell} described in [rfg()]. Default `d.min = 0.1`.
#' @param d.max Upper limit for singular values \eqn{\sqrt{d_j}} used to generate the 
#'    covariance matrix \eqn{V_\ell} described in [rfg()]. Default `d.max = 2`.
#' @param seed Optional seed for reproducibility of random parameter generation.
#'
#' @return A list of length `num.funs`, where each element contains the following parameters used
#'   to specify one basis function:
#'   \item{`a`}{Linear coefficient uniformly sampled as \eqn{a\sim \text{Uniform}[\code{a.min}, \code{a.max}]}.}
#'   \item{`phi`}{Vector of indices indicating the random subset and permutation of input variables to
#'   be used by the basis function.}
#'   \item{`mu`}{Mean vector for the Gaussian function, entries drawn from standard normal distribution.}
#'   \item{`V`}{Covariance matrix \eqn{V \coloneqq U D U^\top}. See [rfg()] for details.}
#'   
#' @seealso [rfg()]
#' @template friedman-reference
#'
#' @keywords internal
generate_rfg_params <- function(p, num.funs = 20L,
                                a.min = -1,
                                a.max = 1,
                                lambda = 2,
                                d.min = 0.1,
                                d.max = 2,
                                seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  params <- lapply(seq_len(num.funs), function(i) {
    r <- rexp(1, 1.0/lambda)
    p.latent <- min(floor(1.5 + r), p)
    U <- generate_orthonormal_matrix(p.latent)
    d <- runif(p.latent, d.min, d.max)^2
    list(
      a   = runif(1, a.min, a.max),
      phi = sample(p, size = p.latent, replace = FALSE),
      mu  = rnorm(p.latent), # Is it worth it to make this more flexible?
      V   = U %*% diag(d, nrow = p.latent, ncol = p.latent) %*% t(U)
    )
  })

  return(params)
}

#' @keywords internal
#' @noRd
generate_orthonormal_matrix <- function(p) {
  qr.Q(qr(matrix(rnorm(p * p), nrow = p, ncol = p)))
}

