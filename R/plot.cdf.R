
plot.cdf <- function(x, emm) {
  df <- data.frame(x = x, y = _cdf(x, emm))
  ggplot(data=df, aes(x=x, y=y)) +
    geom_line() + labs(y = "F(x)") +
    ggtitle("Cumulative distribution function value, F(x)") +
    theme(plot.title = element_text(size=8))
}
