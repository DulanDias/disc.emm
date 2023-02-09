_marginal_log_likelihood <- function(x, pi, mu){
  log_gamma <- log(pi) - log(mu) - x / mu
  each_log_likelihood <- log(sum(exp(log_gamma)))
  return(sum(each_log_likelihood))
}
