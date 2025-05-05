library(rlang)
library(rfg)
rm(list = ls()); gc()

n <- 10000
p <- 2
q <- 2

g <- rfg(p, q, seed = 1)

X <- matrix(rnorm(n * p), nrow = n)
Y <- g(X)

plotter <- function(formula, xlim = range(X), 
                    ylim = range(Y), pars,  ...) {
  if (missing(pars)) {
    pars <- list(
      pch = 20,
      cex = 0.5,
      col = rgb(0, 0, 0, 0.15)
    )
  }
  plot(formula, xlim = xlim, ylim = ylim, type = "n")
  grid(); abline(h = 0, v = 0, lwd = 1.5, col = "gray75")
  inject(points(formula, !!!pars))
}

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
plotter(Y[,1] ~ X[,1])
plotter(Y[,1] ~ X[,2])
plotter(Y[,2] ~ X[,1])
plotter(Y[,2] ~ X[,2])

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
plotter(Y[,1] ~ Y[,2], xlim = range(Y), ylim = range(Y))

