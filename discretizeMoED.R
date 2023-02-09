discretizeMoED <- function(emm.fit, x) {

  intersections <- _find_intersections(pu = emm.fit$pu, mu = emm.fit$mu)

  cuts <- _cut(x, intersections)

  mi <- _mutual_information(x, cuts)

  return(list(cuts = cuts, mi = mi))
}
