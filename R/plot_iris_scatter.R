# scatter plot of two numeric iris columns
plot_iris_scatter <- function(data = iris, xvar = "Sepal.Width", yvar = "Sepal.Length") {

  data <- prepare_iris(data)

  # check xvar and yvar
  if (!is.character(xvar) || length(xvar) != 1) stop("xvar must be a string")
  if (!is.character(yvar) || length(yvar) != 1) stop("yvar must be a string")

  for (v in c(xvar, yvar)) {
    if (!v %in% names(data)) stop(paste("Column not found:", v))
    if (!is.numeric(data[[v]])) stop(paste("Column must be numeric:", v))
  }

  ggplot(data, aes_string(x = xvar, y = yvar, color = "Species")) +
    geom_point() +
    labs(
      title = paste(yvar, "vs", xvar, "in the Iris Dataset"),
      x = paste(xvar, "(numeric)"),
      y = paste(yvar, "(numeric)"),
      color = "Species"
    ) +
    theme_minimal()
}
