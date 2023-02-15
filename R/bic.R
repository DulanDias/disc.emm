bic <- function(marginal_log_likelihood, k, n) {
  return(-marginal_log_likelihood + (2*k - 1) * log(n)/2)
}
