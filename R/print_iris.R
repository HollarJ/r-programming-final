# function prints the summary object
print.iris_summary <- function(x, ...) {
  cat("Summary of Sepal Length By Species (irisToolsHollarJ)\n\n")
  print(x$summary)
  invisible(x)
}
