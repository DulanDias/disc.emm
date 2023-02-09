disc.emm <- function(x, emm) {

  intersections <- find_intersections(pu = emm$pu, mu = emm$mu)

  cuts <- cut(x, intersections)

  mi <- mutual_information(x, cuts)

  return(list(cuts = cuts, mi = mi))
}
