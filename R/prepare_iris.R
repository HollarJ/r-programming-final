# prepare_iris function
prepare_iris <- function(data = iris) {

  # check that data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # if species exists, make it a factor
  if ("Species" %in% names(data)) {
    data$Species <- as.factor(data$Species)
  }

  return(data)
}
