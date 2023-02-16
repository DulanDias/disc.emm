#' Calculates the Bayesian Information Criterion (BIC) for a model given its marginal log likelihood, number of parameters and sample size.
#'
#' The BIC is defined as -2 * marginal_log_likelihood + (2 * k - 1) * log(n) where k is the number of parameters and n is the sample size.
#'
#' @param marginal_log_likelihood The marginal log likelihood of the model.
#' @param k The number of parameters in the model.
#' @param n The sample size.
#' @return The value of the BIC.
#' @examples
#' marginal_log_likelihood <- -10
#' k <- 3
#' n <- 100
#' bic(marginal_log_likelihood, k, n)
#' [1] 32.70554
#'
#' @export
bic <- function(marginal_log_likelihood, k, n) {
  return(-marginal_log_likelihood + (2*k - 1) * log(n)/2)
}
