% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_rfg_params.R
\name{generate_rfg_params}
\alias{generate_rfg_params}
\title{Generate Random Function Generator Parameters}
\usage{
generate_rfg_params(
  p,
  num.funs = 20L,
  a.min = -1,
  a.max = 1,
  lambda = 2,
  d.min = 0.1,
  d.max = 2,
  seed = NULL
)
}
\arguments{
\item{p}{Input dimensionality, number of input variables.}

\item{num.funs}{Number of basis functions to use for the random function generation.
Corresponds to the parameter \eqn{L} described in \code{\link[=rfg]{rfg()}}. Default \code{num.funs = 20}.}

\item{a.min}{Lower limit for the uniform distribution of linear coefficients \eqn{a_\ell}.
Default \code{a.min = -1}.}

\item{a.max}{Upper limit for the uniform distribution of linear coefficients \eqn{a_\ell}.
Default \code{a.max = 1}.}

\item{lambda}{Rate parameter for the exponential random variable \eqn{r\sim \text{Exp}(\lambda)}
described in \code{\link[=rfg]{rfg()}}. Default \code{lambda = 2}.}

\item{d.min}{Lower limit for singular values \eqn{\sqrt{d_j}} used to generate the
covariance matrix \eqn{V_\ell} described in \code{\link[=rfg]{rfg()}}. Default \code{d.min = 0.1}.}

\item{d.max}{Upper limit for singular values \eqn{\sqrt{d_j}} used to generate the
covariance matrix \eqn{V_\ell} described in \code{\link[=rfg]{rfg()}}. Default \code{d.max = 2}.}
}
\value{
A list of length \code{num.funs}, where each element contains the following parameters used
to specify one basis function:
\item{\code{a}}{Linear coefficient uniformly sampled as \eqn{a\sim \text{Uniform}[\code{a.min}, \code{a.max}]}.}
\item{\code{phi}}{Vector of indices indicating the random subset and permutation of input variables to
be used by the basis function.}
\item{\code{mu}}{Mean vector for the Gaussian function, entries drawn from standard normal distribution.}
\item{\code{V}}{Covariance matrix \eqn{V \coloneqq U D U^\top}. See \code{\link[=rfg]{rfg()}} for details.}
}
\description{
Generates parameters for the random function generator (RFG) as described in
Friedman (2001).
}
\references{
Friedman, J. H. (2001). Greedy function approximation: A gradient boosting machine.
The Annals of Statistics, 29(5), 1189-1232.
\doi{10.1214/aos/1013203451}
}
\seealso{
\code{\link[=rfg]{rfg()}}
}
\keyword{internal}
