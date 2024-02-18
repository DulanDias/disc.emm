disc.emm <- function(x, emm) {

  intersections <- find_intersections(x = x, pu = emm$pu, mu = emm$mu)

  cuts <- cutIt(x, intersections)

  mi <- mutual_information(x, cuts)

  return(list(cuts = cuts, mi = mi))
}
