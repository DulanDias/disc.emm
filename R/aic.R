#' Calculate Akaike Information Criterion (AIC)
#'
#' Calculates the Akaike Information Criterion (AIC) using the marginal log-likelihood and the number of parameters in the model.
#'
#' @param marginal_log_likelihood A scalar numeric value representing the marginal log-likelihood.
#' @param k A scalar integer value representing the number of parameters in the model.
#'
#' @return A scalar numeric value representing the AIC.
#'
#' @examples
#' marginal_log_likelihood <- -125.6
#' k <- 3
#' aic(marginal_log_likelihood, k)
#'
#' @export
aic <- function(marginal_log_likelihood, k) {
  return(-marginal_log_likelihood + 2 * k - 1)
}
aic <- function(marginal_log_likelihood, k) {
  return(-marginal_log_likelihood + 2 * k - 1)
}
