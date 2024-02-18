

plot.comps <- function(x, emm, ncols = 2, round = 2) {

  y <- 0

  pi <- emm$pi
  mu <- emm$mu
  colors <- rainbow(length(pi))  # Generate colors for each component

  p <- list()

  df_total <- data.frame(x = x, y = pdf(x, emm))
  pPDF <- ggplot(data=df_total, aes(x=x, y=y)) +
    geom_line(color = "black") +
    labs(y = "p(x)") +
    ggtitle("Probability density function, p(x)") +
    labs(
      x = "x",
      y = "p(x)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8)
    )

  for (i in 1:length(pi)) {
    y <- (pi[i] / mu[i]) * exp(-(x / mu[i]))
    df <- data.frame(x = x, y = y)
    # Add component distributions to first plot
    pPDF <- pPDF +
      geom_line(data = df, aes(x = x, y = y), color = colors[i])

    # Create individual plots for each component
    p[[i+1]] <- ggplot(data=df, aes(x=x, y=y)) +
      geom_line(color = colors[i]) +
      ggtitle(
        paste("Component", i, "\n(w =",
              round(pi[i], digits = round), ", lambda =",
              round(mu[i], digits = round), ")")) +
      labs(
        x = "x",
        y = "p(x)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8)
      )
  }

  p[[1]] <- pPDF  # Replace the first plot with the updated plot

  do.call(gridExtra::grid.arrange, c(p, ncol=ncols))

}
