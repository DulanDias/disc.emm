plot.comps <- function(x, emm, ncols = 2, round = 2) {

  y <- 0

  pi <- emm$pi
  mu <- emm$mu

  p <- list()

  for (i in 1:length(pi)) {
    y <- (pi[i] / mu[i]) * exp(-(x / mu[i]))

    df <- data.frame(x = x, y = y)
    p[[i]] <- ggplot(data=df, aes(x=x, y=y)) +
      geom_line() +
      ggtitle(
        paste("Component", i, "\n(pi =",
              round(pi[i], digits = round), ", mu =",
              round(mu[i], digits = round), ")")) +
      theme(plot.title = element_text(size=8))
  }

  do.call(grid.arrange, c(p, ncol=ncols))

}
