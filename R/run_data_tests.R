#' Run Data Tests.
#'
#' Run all tests for a data set ...
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'
#'@return Creates a test result icon. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
run_data_tests <- function(.path = getwd(), name) {
  # input validation

  test_path <- file.path(.path, "tests", paste0("test-", name, ".R"))
  icon_result_path <- file.path(.path, "tests", paste0("result-", name, ".svg"))
  test_out <- testthat::test_file(test_path)
  test_out_df <- as.data.frame(test_out)

  if(sum(test_out_df$failed == 1) == 0) {
    # create pass icon
    icon_source <- system.file("extdata", "pass_icon.svg", package = "eatDataTest")
  } else {
    icon_source <- system.file("extdata", "fail_icon.svg", package = "eatDataTest")
  }
  file.copy(from = icon_source, to = icon_result_path, overwrite = TRUE)

  invisible(return())
}
