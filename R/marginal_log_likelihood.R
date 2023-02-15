marginal_log_likelihood <- function(x, pi, mu) {
  k <- length(pi)
  log_pi <- log(pi)
  log_mu <- log(mu)
  log_likelihood <- matrix(0, nrow = length(x), ncol = k)
  for (i in 1:k) {
    log_likelihood[, i] <- dexp(x, rate = exp(-log_mu[i]))
  }
  log_likelihood <- log_likelihood + log_pi
  log_likelihood <- rowSums(log_likelihood)
  return(sum(log_likelihood))
}
