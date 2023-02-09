plot.pdf <- function(x, emm) {
  df <- data.frame(x = x, y = pdf(x, emm))
  ggplot(data=df, aes(x=x, y=y)) +
    geom_line() + labs(y = "p(x)") +
    ggtitle("Probability density function value, p(x)") +
    theme(plot.title = element_text(size=8))
}
