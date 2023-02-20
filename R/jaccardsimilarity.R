jaccard_similarity <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  jaccard_coef <- intersection / union
  return(jaccard_coef)
}
