# Random Function Generator

Implementation of the "random function generator" used in Friedman (2001) for synthetic data experiments.

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Usage](#usage)
4. [TODO](#todo)
5. [References](#references)

## Introduction

TODO

## Installation

```R
devtools::install_github("dfleis/rfg")
```

## Usage

TODO

## TODO

* Add informative error messaging when a user tries to supply a vector input to `g <- RFG(p)` rather than a matrix input. If `g <- rfg(p)` then the generated function `g()` expects an input `x` to be a matrix with `p` columns. If user tries to pass a vector, say `x <- rnorm(p)` with the intention that it represents a single $p$-dimensional vector, then the function currently errors out and displays the message `Error in x[, params[[l]]$phi, drop = FALSE] : incorrect number of dimensions`.
  
    It would be nice to either add clear instructions on how to resolve this error, or allow vector-valued inputs under the assumption that they are length-$p$ (maybe printing a warning messaging that the function is treating it as a single observation).
* Replace the `incl.params` mechanism in `rfg()` with an optional user-supplied environment via some new argument `env = NULL`. If `incl.params = TRUE` then `rfg()` adds the generated parameters as attributes on the function. Instead, I think it would be preferable to store the parameters in some environment such that advanced users could be able to modify the parameters after generation-time.
* Allow more flexible generation for the the mean vectors $\mu_\ell$: The RFG mechanism used by Friedman (2001) specifies that the mean vectors $\mu_\ell$ be sampled from the same distribution as the inputs $x$. In this implementation, the mean vectors are independently sampled as standard Gaussians, $\mu_\ell \sim \mathcal N({\bf 0},\mathbb I)$.  One idea is to keep the $\mu_\ell$ as Gaussian, but allow users to pass a mean (i.e. a mean for the mean vectors) and covariance, i.e. $\mu_\ell \sim \mathcal N(\mu^\star, \Sigma_\mu^\star)$ where $\mu^\star$ and $\Sigma_\mu^\star$ are optionally user-supplied. An even more flexible option is to allow the user to supplied a random sampler that generates random vectors of the appropriate size.

## References
Jerome H. Friedman.
<b>Greedy function approximation: A gradient boosting machine.</b>
<i>The Annals of Statistics</i>, 29, 2001.
[<a href="https://doi.org/10.1214/aos/1013203451">paper</a>]
