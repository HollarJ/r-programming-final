# function creates a summary table and makes a custom s3 object
summarize_iris <- function(data = iris) {

  data <- prepare_iris(data)

  summary_tbl <- data |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      mean_sepal_length = mean(Sepal.Length),
      sd_sepal_length   = sd(Sepal.Length),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    tibble::as_tibble()

  result <- list(
    summary = summary_tbl,
    vars = c("Sepal.Length", "Species"),
    call = match.call()
  )

  class(result) <- "iris_summary"

  return(result)
}
