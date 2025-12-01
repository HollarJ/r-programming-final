# finds outliers in a numeric vector using the tukey rule
tukey.outlier <- function(x) {

  # check numeric
  if (!is.numeric(x)) stop("x must be numeric")

  q1 <- stats::quantile(x, 0.25)
  q3 <- stats::quantile(x, 0.75)
  iqr <- q3 - q1

  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr

  return(x < lower | x > upper)
}

tukey_multiple_safe <- function(x) {

  # defensive checks
  if (!is.matrix(x)) stop("x must be a matrix")
  if (!is.numeric(x)) stop("x must be a numeric matrix")
  if (nrow(x) == 0 || ncol(x) == 0) stop("x cannot be empty")

  outliers <- array(TRUE, dim = dim(x))

  for (j in 1:ncol(x)) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }

  result <- logical(nrow(x))

  for (i in 1:nrow(x)) {
    result[i] <- all(outliers[i, ])
  }

  return(result)
}
