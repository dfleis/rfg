#' @title Random Function Generator
#'
#' @description Creates a random function according to the design in Friedman 
#'   (2001). The function is a sum of multivariate Gaussian basis functions
#'   with random parameters, intended for evaluating algorithms on synthetic data.
#'   
#' @details The random function generator creates a complex function over
#'   multivariate inputs defined as a linear combination of nonlinear basis
#'   functions:
#'   
#'   \deqn{F^*(x) \coloneqq \sum_{\ell=1}^{L} a_\ell g_\ell(z_\ell),}
#'   
#'   where,
#'   
#'   \itemize{
#'     \item \eqn{L} is the number of basis functions,
#'     \item each coefficient \eqn{a_\ell} is generated from a uniform distribution,
#'     \item each \eqn{g_\ell(z_\ell)} is a Gaussian function on a random subset
#'        \eqn{z_\ell} of \eqn{x},
#'     \item each subset \eqn{z_\ell} consists of \eqn{p_\ell} randomly permuted variables
#'        drawn from \eqn{x} (without replacement), and
#'     \item subset size \eqn{p_\ell \coloneqq \min\{\lfloor 1.5 + r \rfloor, p\}} where 
#'        \eqn{r \sim \text{Exp}(\lambda)}, parameterized such the that expected value
#'        of \eqn{r} is \eqn{\mathbb E[r] = 1/\lambda}. Therefore, when the number of
#'        input variables \eqn{p} is large, the expected number of input variables used
#'        by a function \eqn{g_\ell(\cdot)} is between \eqn{\lfloor 1 + 1/\lambda \rfloor}
#'        and \eqn{\lfloor 2 + 1/\lambda \rfloor}.
#'   }
#'   
#'   Each basis function \eqn{g_\ell(\cdot)} is a Gaussian function and takes the form:
#'   
#'   \deqn{g_\ell(z_\ell) \coloneqq \exp\left(-\frac{1}{2}(z_\ell - \mu_\ell)^\top V_\ell (z_\ell - \mu_\ell)\right),}
#'   
#'   where,
#'   
#'   \itemize{
#'     \item mean vector \eqn{\mu_\ell} entries are drawn from a standard Gaussian distribution
#'     \item covariance matrix \eqn{V_\ell \coloneqq U_\ell D_\ell U_\ell^\top} with 
#'     \itemize{
#'        \item random \eqn{p_\ell\times p_\ell} orthonormal matrix \eqn{U_\ell} (eigenvectors), and
#'        \item diagonal matrix \eqn{D_\ell} with diagonal entries \eqn{d_j > 0} (eigenvalues) such
#'           that \eqn{\sqrt{d_j}} is generated uniformly. Note that it is \eqn{\sqrt{d_j}} that
#'           is generated uniformly (the singular values) and not \eqn{d_j} (the eigenvalues).
#'     }
#'   }
#'
#' @inheritParams generate_rfg_params
#' @param ... Additional arguments passed to [generate_rfg_params()].
#' @param incl.params Logical value indicating whether the parameters used
#'   to generate the function should be included as attributes of the
#'   generated function. Default `incl.params = FALSE`.
#'
#' @return A function that takes a matrix `x` of observations (rows) and 
#'   variables (columns) as input, and returns a numeric vector such that
#'   each entry is the output of the random function evaluated on the
#'   corresponding row of `x`.
#' 
#' @seealso [generate_rfg_params()]
#' @template friedman-reference
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' n <- 1000
#' p <- 2
#' X <- matrix(rnorm(n * p), ncol = p)
#'
#' g <- rfg(p = p)
#' y <- g(X)
#' 
#' #----- plot
#' # plot(y ~ X[,1])
#' # plot(y ~ X[,2])
#' }
#' 
#' @export
rfg <- function(p, num.funs = 20L, ...,
                incl.params = FALSE) {
  params <- generate_rfg_params(p, num.funs = num.funs, ...)

  FUN <- function(x) {
    out <- rep(0, NROW(x))
    for (l in seq_len(num.funs)) {
      # Subset/permute the input covariate matrix, then center by mu
      zc <- sweep(x[, params[[l]]$phi, drop = FALSE], 2, params[[l]]$mu, "-")
      zV <- zc %*% params[[l]]$V
      # Compute quadratic form, then Gaussian
      out <- out + params[[l]]$a * exp(-0.5 * rowSums(zV * zc))
    }
    return(out)
  }

  if (isTRUE(incl.params)) attr(FUN, "params") <- params
  return(FUN)
}
