_cut <- function(continuous_var, cutpoints) {
  return(cut(continuous_var,  breaks = c(-Inf, cutpoints, Inf)))
}
