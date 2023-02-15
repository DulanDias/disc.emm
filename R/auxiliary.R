aux.dot <- function(x, y) {   # x and y can be vectors or matrices
  result <- t(x)%*%y   # %*% is the matrix multiplication operator
  return(result)        # t(x) denotes the transpose of x
}

aux.encode <- function(k, z) {

  d <- diag(nrow = k)
  za <- unique(sort(z))

  result <- matrix(0, length(z), k)

  for (i in 1:length(z)) {
    indices <- which(za == z[i])
    if (length(indices) > 0) {
      result[i, ] <- d[indices, ]
    } else {
      result[i, ] <- rep(0, k)
    }
  }

  return(result)
}
