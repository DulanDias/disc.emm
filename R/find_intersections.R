find_intersections <- function(pu, mu, tolerance = 1e-10){
  intersections <- c()
  for(i in 1:(length(mu)-1)){
    for(j in (i+1):length(mu)){
      x_l <- 0
      x_u <- max(x)
      f_l <- pu[i]*exp(-mu[i]*x_l) - pu[j]*exp(-mu[j]*x_l)
      f_u <- pu[i]*exp(-mu[i]*x_u) - pu[j]*exp(-mu[j]*x_u)
      while(x_u - x_l > tolerance){
        x_mid <- (x_u + x_l) / 2
        f_mid <- pu[i]*exp(-mu[i]*x_mid) - pu[j]*exp(-mu[j]*x_mid)
        if(f_mid == 0){
          break
        } else if(f_mid * f_l < 0){
          x_u <- x_mid
          f_u <- f_mid
        } else {
          x_l <- x_mid
          f_l <- f_mid
        }
      }
      intersections <- c(intersections, x_mid)
    }
  }
  return(intersections)
}
