#' @title Generate a Random Orthonormal Matrix
#'
#' @description
#' Creates an orthonormal matrix based on the QR decomposition of a square
#' matrix with standard Gaussian entries.
#' 
#' @details
#' TODO ... the reason we generate orthonormal matrices this way is because
#' the distribution of the matrix \eqn{Q} from the QR decomposition on the
#' IID Gaussian matrix \eqn{M = QR} will be uniform over the space of
#' orthonormal matrices \eqn{Q \in O(p)} ... this means that the resulting
#' matrix \eqn{Q} can be thought of as a random rotation/reflection matrix
#' that is unbiased over the space of possible rotation/reflection matrices
#' in the sense of having no preferred rotational direction ...
#' 
#' @param p The dimensionality of the desired matrix.
#' 
#' @return A `p`-by-`p` orthonormal matrix.
#' 
#' @keywords internal
generate_orthonormal_matrix <- function(p) {
  qr.Q(qr(matrix(stats::rnorm(p * p), nrow = p, ncol = p)))
}

#' @title Execute Code with an Optional Random Seed
#'
#' @description
#' A simple wrapper around [withr::with_seed()], designed to avoid messages
#' caused by a missing or `NULL` seed.
#' 
#' @param code The expression to evaluate.
#' @param seed An optional numeric seed.
#' 
#' @keywords internal
with_optional_seed <- function(code, seed) {
  if (missing(seed) || is.null(seed)) {
    code
  } else {
    withr::with_seed(seed = seed, code = code)
  }
}
