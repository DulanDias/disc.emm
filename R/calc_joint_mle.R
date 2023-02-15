calc_joint_mle <- function(x, z) {

  n <- length(x)

  counts <- tabulate(z)
  z <- match(z, cumsum(counts > 0))
  n_each_exp <- counts[counts > 0]
  k_final <- length(n_each_exp)

  pi <- n_each_exp / n
  mu <- t(sapply(split(x, z), mean))

  mu_ascend <- order(mu)
  z <- mu_ascend[z]
  n_each_exp <- n_each_exp[mu_ascend]
  pi <- pi[mu_ascend]
  mu <- mu[mu_ascend, , drop = FALSE]

  joint_log_likelihood <- joint_log_likelihood(n, n_each_exp, pi, mu)

  return(list(pi = pi, mu = mu, z = z, n_each_exp = n_each_exp,
              k_final = k_final, joint_log_likelihood = joint_log_likelihood))
}
