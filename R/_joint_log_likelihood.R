_joint_log_likelihood <- function(n, n_each_exp, pi, mu){
  temp <- 0
  for(j in seq_along(n_each_exp)){
    if(n_each_exp[j] > 0){
      temp <- temp + n_each_exp[j] * (log(pi[j]) - log(mu[j]))
    }
  }
  return(temp - n)
}
