#' Add Data.
#'
#' Add the infrastructure for a new data set to the `eatDaT` repository. This includes
#' * a `.yaml` file
#' * a markdown changelog (`.md` file)
#' * a test file (`.R` file in `tests`)
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. All files will be named accordingly.
#'@param release_path Path to the data set.
#'@param oldrel_path Optional: Path to the previous data version.
#'@param version Version number of the current data set. Defaults to `'v1.0'`.
#'
#'@return Creates data set infrastructure. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
add_data <- function(.path = getwd(), name, release_path, oldrel_path = "", version = "v1.0") {
  # input validation

  file_path <- file.path(.path, "data", paste0(name, ".yaml"))

  # yaml
  create_data_yaml(.path = .path, name = name, release_path = release_path, oldrel_path = oldrel_path,
                   version = version)

  # markdown changelog
  create_changelog(.path = .path, name = name, version = version)

  # test file
  create_tests(.path = .path, name = name)

  # initialized test result icon (fail)
  create_initial_test_result(.path = .path, name = name)

  # markdown entry (copy & paste?) for readem
  create_readme_snippet(.path = .path, name = name, version = version)
  # TODO: create markdown entry for readme; how to paste this to clipboard?

  invisible(return())
}

# TODO: alternatively use templates like usethis (store in inst and copy paste/modify?)

create_changelog <- function(.path = getwd(), name, version = "v1.0") {
  # input validation

  file_path <- file.path(.path, "changelogs", paste0(name, ".md"))

  md_content <- paste0("# ", version,
                       "\n-initial release\n")

  writeLines(md_content, con = file_path, sep = "")
  invisible(return())
}

create_tests <- function(.path = getwd(), name) {
  # input validation

  file_path <- file.path(.path, "tests", paste0("tests-", name, ".R"))

  r_content <- paste0('dat <- import_data(name = "', name, '", data_version = "release")\n',
                      '\n',
                       'test_that("test GADSdat structure", {\n',
                        '  expect_is(dat, "GADSdat")\n',
                        '})')

  writeLines(r_content, con = file_path)
  invisible(return())
}

create_initial_test_result <- function(.path = getwd(), name) {
  # input validation

  icon_result_path <- file.path(.path, "tests", paste0("result-", name, ".svg"))
  icon_source <- system.file("extdata", "fail_icon.svg", package = "eatDataTest")

  file.copy(from = icon_source, to = icon_result_path)
  invisible(return())
}

create_readme_snippet <- function(.path = getwd(), name, version = "v1.0") {
  # input validation

  md_content <- "| kind_ebene              | v1.0    | ![s](tests/testthat/icons8-success.svg) |"

  md_content
}


