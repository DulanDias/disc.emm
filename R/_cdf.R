#' Cumulative distribution function value, F(x), at each element, x_i, of x.
cdf <- function(x, emm) {
  y <- 0

  pi <- emm$pi
  mu <- emm$mu

  for (i in 1:length(pi)) {
    y <- y + (pi[i]) * (1 - exp(-(x / mu[i])))
  }

  return(y)
}
