mutual_info <- function(x, y) {# x = continuous and y = binned

  y <- as.factor(y)
  nbins <- levels(y)

  # calculate marginal probabilities of x and y
  p_x <- prop.table(table(x))
  p_y <- prop.table(table(y))

  # calculate joint probabilities of x and y
  p_xy <- table(x, y) / length(x)

  # initialize mutual information
  mi <- 0

  # loop over bins of x and levels of y
  for (i in x) {
    for (j in 1:length(nbins)) {
      # get probability of x bin i and y level j
      p_xy_ij <- p_xy[i, j]

      # get probabilities of x bin i and y level j separately
      p_x_i <- p_x[i]
      p_y_j <- p_y[j]

      # add mutual information for this bin/level combination
      if (p_xy_ij > 0) {
        mi <- mi + p_xy_ij * log2(p_xy_ij / (p_x_i * p_y_j))
      }
    }
  }

  # return mutual information
  return(mi)
}
