marginal_log_likelihood <- function(x, pi, mu) {

  log_gamma <- t(replicate(length(x), pi)) - t(replicate(length(x), mu)) - matrix(x, ncol = 1) %*%  (1 / matrix(mu, nrow = 1))

  each_log_likelihood <- apply(log_gamma, 1, function(x) logsumexp(x))

  return(sum(each_log_likelihood))
}
