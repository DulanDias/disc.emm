run_em <- function(x, k, mu0 = NULL, n_iter = 1000) {
  if (is.null(mu0)) {
    mu0 <- 10^(seq(log10(min(x)), log10(max(x)), length.out = k))
  }

  n <- length(x)
  pi <- rep(1/k, k)
  mu <- mu0
  log_pi <- log(pi)
  log_mu <- log(mu)
  log_x <- matrix(log(x), nrow = n, ncol = 1)

  for (i in 1:n_iter) {
    # E step
    log_gamma_temp <- matrix(log_pi, nrow = n, ncol = length(log_pi), byrow = TRUE) - matrix(log_mu, nrow = n, ncol = length(log_mu), byrow = TRUE) - matrix(x, ncol = 1) %*% (1 / matrix(exp(log_mu), nrow = 1, ncol = length(log_mu)))
    log_gamma_temp <- log_gamma_temp - matrix(rowSums(exp(log_gamma_temp)), nrow = length(rowSums(exp(log_gamma_temp))), ncol = ncol(log_gamma_temp))

    # Check if log_gamma_temp contains -Inf values
    if (any(is.infinite(log_gamma_temp))) {
      warning(paste("EM algorithm reached -Inf with the provided number of iterations. Completed ", i, " out of ", n_iter, " iterations only.\n"))
      break  # Break the loop if any matrix has -Inf values
    } else {
      log_gamma <- log_gamma_temp
    }

    # M step
    log_pi_temp <- rowSums(log_gamma) - log(n)
    log_mu_temp <- rowSums(log_gamma + cbind(log_x, matrix(rep(log_x[, 1], ncol(log_gamma) - 1), nrow = nrow(log_x)))) - rowSums(log_gamma)

    # Check if log_pi_temp or log_mu_temp contains -Inf values
    if (any(is.infinite(log_pi_temp)) || any(is.infinite(log_mu_temp))) {
      warning(paste("EM algorithm reached -Inf with the provided number of iterations. Completed ", i, " out of ", n_iter, " iterations only.\n"))
      break  # Break the loop if any matrix has -Inf values
    } else {
      log_pi <- log_pi_temp
      log_mu <- log_mu_temp
    }
  }

  log_gamma <- matrix(log_pi, nrow = n, ncol = length(log_pi), byrow = TRUE) - matrix(log_mu, nrow = n, ncol = length(log_mu), byrow = TRUE) - matrix(x, ncol = 1) %*% (1 / matrix(exp(log_mu), nrow = 1, ncol = length(log_mu)))
  z <- apply(log_gamma, 1, which.max)

  marginal_log_likelihood <- marginal_log_likelihood(x, exp(log_pi), exp(log_mu))


  warning(paste("\n\n\n", marginal_log_likelihood, "\n\n\n"))

  return(list(z = z, marginal_log_likelihood = marginal_log_likelihood))
}
