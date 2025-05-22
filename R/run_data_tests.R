#' Run Data Tests.
#'
#' Run all tests for a data set.
#'
#'@param .path Path to the `eatDataTest` repository. Defaults to the current working directory.
#'@param name Name of the data set.
#'@param overwrite_icon Should the result icon in the `tests` folder be overwritten?
#'@param overwrite_version Should the version number in the `tests` folder be overwritten?
#'
#'@return Creates a test result icon. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
run_data_tests <- function(.path = getwd(), name, overwrite_icon = TRUE, overwrite_version = TRUE) {
  # input validation

  test_path <- file.path(.path, "tests", paste0("test-", name, ".R"))
  icon_result_path <- file.path(.path, "tests", paste0("result-", name, ".svg"))
  version_path <- file.path(.path, "tests", paste0("version-", name, ".svg"))

  cli::cli_alert_info(paste0("Running tests for data set ", name, ":"))
  icon_source <- run_tests_via_testthat(test_path = test_path)

  if(overwrite_icon) {
    cli::cli_alert_info(paste0("Updating icon..."))
    file.copy(from = icon_source, to = icon_result_path, overwrite = TRUE)
  }

  if(overwrite_version) {
    cli::cli_alert_info(paste0("Updating version..."))
    version_number <- get_version(.path = .path, name = name)
    create_and_download_badge(gsub("v", "", version_number), download_path = version_path)
  }

  invisible(NULL)
}

run_tests_via_testthat <- function(test_path) {
  #browser()
  test_out <- testthat::test_file(test_path)

  test_out_df <- as.data.frame(test_out)
  if(sum(test_out_df$failed == 1) == 0 && # regular failures
     sum(test_out_df$error) == 0) { # outside of test_that calls
    cli::cli_alert_success(paste0("All tests passing!"))
    icon_source <- system.file("extdata", "pass_icon.svg", package = "eatDataTest")
  } else {
    cli::cli_alert_danger(paste0("Some tests failing!"))
    icon_source <- system.file("extdata", "fail_icon.svg", package = "eatDataTest")
  }
  icon_source
}

create_and_download_badge <- function(version_number, download_path) {
  prefix <- "version"
  color <- "black"
  badge_url <- paste0("https://img.shields.io/badge/", prefix, "-",
                  version_number, "-", color, ".svg")
  download.file(url = badge_url, destfile = download_path, method = "curl", quiet = TRUE)
  invisible(NULL)
}
