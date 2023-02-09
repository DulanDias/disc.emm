discretizeEmm <- function(x, emm) {

  intersections <- _find_intersections(pu = emm$pu, mu = emm$mu)

  cuts <- _cut(x, intersections)

  mi <- _mutual_information(x, cuts)

  return(list(cuts = cuts, mi = mi))
}
