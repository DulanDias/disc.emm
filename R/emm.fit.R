#' Exponential Mixture Model
#'
#' Fit an EMM to data 'x'. EM algorithm is used for fitting. The EM algorithm is run several times to avoid local maxima as much as possible.
#' @param x Numerical vector. Data.
#' @param k int, optional. Number of components initially given.
#' @param n_init int, optional. EM algorithm is run n_init times with different initial conditions. The estimated parameters that maximize the joint likelihood are selected.
#' @param n_iter int, optional. Number of iterations in the EM algorithm.
#' @param seed int, 1 (default). Seed of random number generator.
#' @return List with pi (vector - Mixing weight of the individual exponential distribution in the estimated EMM. The vector length is k_final.) and mu (vector - Mean parameter of the individual exponential distribution in the estimated EMM. The vector length is k_final.).
#' @keywords emm, exponential mixture model

emm.fit <- function(x, k = 10, n_init = 10, n_iter = 1000, seed = 1) {

  set.seed(seed)

  # package names
  packages <- c("matrixStats", "ggplot2", "gridExtra", "dplyr")

  # install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # packages loading
  invisible(lapply(packages, library, character.only = TRUE))

  # parameters
  pi <- NULL # mixing weight
  mu <- NULL # mean of the exponential distribution

  # latent variables
  z <- NULL
  n_each_exp <- NULL  # number of samples that belong to each component
  k_final <- NULL  # effective number of components

  # data
  n = NULL  # length of data

  # empirical log likelihood
  marginal_log_likelihood = NULL  # empirical marginal log likelihood
  joint_log_likelihood = NULL  # empirical joint log likelihood

  # x as a vector
  if(!(is.vector(x) && is.numeric(x))) {
    stop("x should be a vector of class numeric.")
  }

  # remove '0' element if exist in x
  if(0 %in% x) {
    x <- x[x > 0]
    print("'x' contains 0, which has been removed.")
  }

  # extract unique values from vector x
  x <- unique(x)

  # set length of the vector
  n <- length(x)

  max_log_likelihood <- -Inf

  for (i in 1:n_init) {
    mu0 <- 10^runif(k, log10(min(x)), log10(max(x)))
    z <- run_em(x, k, n_iter = n_iter, mu0 = mu0)
    temp_results <- calc_joint_mle(x, z)
    if (temp_results[["joint_log_likelihood"]] > max_log_likelihood) {
      max_log_likelihood <- temp_results[["joint_log_likelihood"]]
      marginal_log_likelihood <- temp_results[["marginal_log_likelihood"]]
      pi <- temp_results[["pi"]]
      mu <- temp_results[["mu"]]
      z <- temp_results[["z"]]
      n_each_exp <- temp_results[["n_each_exp"]]
      k_final <- temp_results[["k_final"]]
      joint_log_likelihood <- temp_results[["joint_log_likelihood"]]
    }
  }

  ## evaluation metrics

  aic <- aic(marginal_log_likelihood, k)
  bic <- bic(marginal_log_likelihood, k, n)

  return(list(pi = pi, mu = mu, aic = aic, bic = bic))
}
