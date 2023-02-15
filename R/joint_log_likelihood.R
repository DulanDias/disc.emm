joint_log_likelihood <- function(n, n_each_exp, pi, mu) {
  k <- length(pi)
  likelihood <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    likelihood[, i] <- dexp(x, rate = 1/mu[i])
  }
  joint_likelihood <- likelihood %*% pi
  joint_log_likelihood <- sum(log(joint_likelihood))
  return(joint_log_likelihood)
}
