logsumexp <- function(x) {
  a <- max(x)
  return(a + log(sum(exp(x - a))))
}

