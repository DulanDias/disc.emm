logsumexp <- function(x) {
  a <- max(x)
  a + log(sum(exp(x - a)))
}

