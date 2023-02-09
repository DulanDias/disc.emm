#' Akaike information criterion for the estimated EMM.
aic <- function(marginal_log_likelihood, k) {
  return(-marginal_log_likelihood + 2 * k - 1)
}
