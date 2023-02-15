marginal_log_likelihood <- function(x, pi, mu) {
  log_gamma <- log(matrix(pi, nrow = n, ncol = length(pi), byrow = TRUE)) - log(matrix(mu, nrow = n, ncol = length(mu), byrow = TRUE)) - matrix(x, ncol = 1) %*% (1 / matrix(mu, nrow = 1, ncol = length(mu)))

  each_log_likelihood <- apply(log_gamma, 1, function(x) logsumexp(x))
  return(sum(each_log_likelihood))
}
