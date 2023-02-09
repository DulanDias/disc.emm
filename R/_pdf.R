#' Probability density function value, p(x), at each element, x_i, of x.
pdf <- function(x, emm) {
  y <- 0

  pi <- emm$pi
  mu <- emm$mu

  for (i in 1:length(pi)) {
    y <- y + (pi[i] / mu[i]) * exp(-(x / mu[i]))
  }

  return(y)
}
