library(S7)

.always_true <- function(x) TRUE

prop_numeric <- function(name = NULL, check = .always_true, ...) {
  S7::new_property(
    name = name,
    class = S7::class_numeric,
    validator = function(value) {
      if (!isTRUE(check(value))) {
        paste("Underlying data must be a", name)
      }
    },
    ...
  )
}
prop_numeric_vector <- prop_numeric("vector", function(x) length(x) > 0L)
prop_numeric_scalar <- prop_numeric("scalar", function(x) length(x) == 1L)
prop_numeric_matrix <- prop_numeric("matrix", is.matrix)

rfg_basis_params <- S7::new_class(
  name = "rfg_basis_params",
  properties = list(
    a   = prop_numeric_scalar,
    phi = prop_numeric_vector, # should be strictly positive integer indices
    mu  = prop_numeric_vector,
    V   = prop_numeric_matrix
  ),
  validator = function(self) {
    if (NROW(self@V) != NCOL(self@V)) {
      "@V must be a square matrix"
    } else if (length(self@phi) != length(self@mu)) {
      "The length of @phi must be equal to the length of @mu"
    } else if (length(self@phi) != NROW(self@V)) {
      "The length of @phi must agree with the rows (columns) of @V"
    } else if (length(self@mu) != NROW(self@V)) {
      "The length of @mu must agree with the rows (columns) of @V"
    }
  }
)

# prop_p <-  S7::new_property(
#   class = S7::class_integer,
#   setter = function(self, value) {
#     self@p <- as.integer(value)
#     self
#   }
# )
prop_p <- S7::new_property(
  name = "p",
  class = S7::class_numeric,     # should be strictly positive integer index
  validator = function(value) {  # maybe just do as.integer() in a setter?
    if (length(value) != 1L) {
      "Must be an integer value representing the dimensionality of the covariate space"
    }
  }
)
prop_bases <- S7::new_property(
  name = "bases",
  class = S7::class_list,
  validator = function(value) {
    if (length(value) == 0L) {
      "Must be a nonempty list of <rfg_basis_params> objects"
    } else if (!all(sapply(value, S7::S7_inherits, class = rfg_basis_params))) {
      "Must be a list of only <rfg_basis_params> objects"
    }
  }
)
rfg_params <- S7::new_class(
  name = "rfg_params",
  properties = list(
    p = prop_p,
    bases = prop_bases # list of rfg_basis_params
  ),
  validator = function(self) {
    for (idx in seq_along(self@bases)) {
      basis_params <- self@bases[[idx]]

      if (any(basis_params@phi > self@p)) {
        msg <- sprintf(
          "Basis function %i uses a covariate index (%i) in @phi that will exceed the covariate dimensionality @p (%i)",
          idx, max(basis_params@phi), self@p
        )
        return (msg)
      }
    }
  }
)

generate_orthonormal_matrix <- function(p) {
  qr.Q(qr(matrix(stats::rnorm(p * p), nrow = p, ncol = p)))
}
generate_rfg_params <- function(
    p,
    num.funs = 20L,
    a.min = -1,
    a.max = 1,
    lambda = 2,
    d.min = 0.1,
    d.max = 2) {

  bases <- replicate(num.funs, {
    r <- stats::rexp(1, 1.0/lambda)
    p_latent <- min(floor(1.5 + r), p)
    U <- generate_orthonormal_matrix(p_latent)
    d <- stats::runif(p_latent, d.min, d.max)^2
    rfg_basis_params(
      a   = stats::runif(1, a.min, a.max),
      phi = sample(p, size = p_latent, replace = FALSE),
      mu  = stats::rnorm(p_latent), # TODO pass a `mu.sampler` argument such that `mu = mu.sampler(p.latent)`
      V   = U %*% diag(d, nrow = p_latent, ncol = p_latent) %*% t(U)
    )
  }, simplify = FALSE)

  rfg_params(p = p, bases = bases)
}

prop_rfg_param_sets <- S7::new_property(
  class = S7::class_list,
  validator = function(value) {
    if (!all(sapply(value, S7::S7_inherits, class = rfg_params))) {
      "Must be a list of only <rfg_params> objects"
    }
  }
)
rfg_multivariate_params <- S7::new_class(
  name = "rfg_multivariate_params",
  properties = list(
    param_sets = prop_rfg_param_sets
  ),
  validator = function(self) {
    if (length(self@param_sets) == 0L) {
      "Property @param_sets must be a nonempty list of <rfg_params> objects"
    } else if (length(unique(sapply(self@param_sets, function(params) params@p))) != 1L) {
      "Every <rfg_params> object must have the same covariate dimensionality @p"
    }
  }
)

generate_rfg_multivariate_params <- function(p, q = 1, ...) {
  param_sets <- lapply(seq_len(q), function(i) generate_rfg_params(p = p, ...))
  rfg_multivariate_params(param_sets = param_sets)
}


rfg_function <- S7::new_class(
  name = "rfg_function",
  parent = S7::class_function,
  properties = list(
    params = S7::new_union(rfg_params, rfg_multivariate_params)
  )
)

rfg <- S7::new_generic("rfg", "x")

S7::method(rfg, rfg_params) <- function(x) {
  p <- x@p
  bases <- x@bases

  FUN <- function(.x) {
    if (!isTRUE(NCOL(.x) == p)) {
      stop(paste0("Input `x` must have ", p, " columns"))
    }

    res <- lapply(bases, function(basis_params) {
      zc <- sweep(.x[, basis_params@phi, drop = FALSE], 2, basis_params@mu, "-")
      zV <- zc %*% basis_params@V
      basis_params@a * exp(-0.5 * rowSums(zV * zc))
    })
    return (Reduce(`+`, res))
  }

  rfg_function(FUN, params = x)
}

with_optional_seed <- function(code, seed) {
  # withr::with_seed prints a warning if `seed = NULL`
  # This wrapper avoids such behaviour without suppressing all warnings outright
  if (missing(seed) || is.null(seed)) {
    code
  } else {
    withr::with_seed(seed = seed, code = code)
  }
}

S7::method(rfg, S7::class_numeric) <- function(x, ..., seed = NULL) {
  args <- utils::modifyList(list(...), list(p = x))
  params <- with_optional_seed(code = do.call(generate_rfg_params, args), seed = seed)
  rfg(params)
}


params <- S7::new_generic("params", "x")
S7::method(params, rfg_function) <- function(x) {
  x@params
}



# rfg_multivariate_params  <- S7::new_class(
#   name = "rfg_multivariate_params",
#   parent = S7::class_list,
#   validator = function(self) {
#     if (length(self) == 0L) {
#       "Must be a nonempty list of <rfg_params> objects"
#     } else if (!all(sapply(self, S7::S7_inherits, class = rfg_params))) {
#       "Must be a list of only <rfg_params> objects"
#     } else if (!length(unique(sapply(self, function(params) params@p))) == 1L) {
#       "Every component function must have the same covariate dimensionality @p in its <rfg_params>"
#     }
#   }
# )



S7::method(rfg, rfg_multivariate_params) <- function(x) {
  FUNS <- lapply(x@param_sets, rfg)

  FUN <- function(.x) {
    vapply(FUNS, function(f) f(.x), FUN.VALUE = numeric(nrow(.x)))
  }

  rfg_function(FUN, params = x)
}

# print.rfg_params <- function(x, ...) {
#   # cat(
#   #   sprintf("<%s>\n", paste(class(x), collapse = ", ")),
#   #   sprintf("- Input Dims (p)  : %d\n", x@p),
#   #   sprintf("- Num. Basis Funs : %d\n", length(x@bases)),
#   #   "- Each basis contains: @a, @phi, @mu, @V\n"
#   # )
#   # invisible(x)
#
# }

#--------------------------------------------------
#----- Testing below
#--------------------------------------------------
set.seed(1)
n <- 5
p <- 3
X <- matrix(rnorm(n * p), nrow = n, byrow = T)
set.seed(1)
my_params <- generate_rfg_params(p)
f <- rfg(my_params)
f(X)
# f
# params(f)

set.seed(1)
n <- 5
p <- 3
X <- matrix(rnorm(n * p), nrow = n, byrow = T)
set.seed(1)
f <- rfg(p)
f(X)
f(X)

rfg(p, seed = 2)(X)

set.seed(1)
f <- rfg(p)
b <- params(f)@bases[[1]]
b

set.seed(1)
f <- rfg(p)
params(f)

params(f)@bases

params(f)

set.seed(1)
n <- 5
p <- 3
q <- 2
X <- matrix(rnorm(n * p), nrow = n, byrow = T)
set.seed(1)
my_params <- generate_rfg_multivariate_params(p = p, q = q)
f <- rfg(my_params)
f(X)



