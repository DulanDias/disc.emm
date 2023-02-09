_run_em <- function(x, k, mu0 = NULL, n_iter = 1000){
  if(is.null(mu0)){
    mu0 <- 10^seq(log10(min(x)), log10(max(x)), length.out = k)
  }
  n <- length(x)
  pi <- rep(1/k, k)
  mu <- as.numeric(mu0)
  log_pi <- log(pi)
  log_mu <- log(mu)
  log_x <- log(x)

  for(i in 1:n_iter){
    # E step
    log_gamma <- log_pi - log_mu - x / exp(log_mu)
    log_gamma <- log_gamma - log(sum(exp(log_gamma)))

    # M step
    log_pi <- log(sum(exp(log_gamma))) - log(n)
    log_mu <- log(sum(exp(log_gamma + log_x))) - log(sum(exp(log_gamma)))
  }

  # estimate z
  log_gamma <- log_pi - log_mu - x / exp(log_mu)
  z <- which.max(log_gamma, arr.ind = T)

  marginal_log_likelihood <- _marginal_log_likelihood(x, exp(log_pi), exp(log_mu))

  return(list(z = z, marginal_log_likelihood = marginal_log_likelihood))
}
