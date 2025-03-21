#' Add Data.
#'
#' Add the infrastructure for a new data set to the `eatDataTest` repository. This includes
#' * creating an empty `GADSdat` object if `oldrel` is empty
#' * a `.yaml` file
#' * a markdown changelog (`.md` file)
#' * a test file (`.R` file in `tests`)
#' * an initial test result icon (`.svg` file in `tests`)
#' * a snippet which can be inserted into the `Readme.md`.
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. All files will be named accordingly.
#'@param release_path Path to the data set.
#'@param oldrel_path Optional: Path to the previous data version.
#'@param version Version number of the current data set. Defaults to `'v1.0'`.
#'
#'@return Creates data set infrastructure. Returns a markdown snippet.
#'
#'@examples
#'## tbd
#'
#'@export
add_data <- function(.path = getwd(), name, release_path, oldrel_path = NULL, version = "v1.0") {
  # input validation

  file_path <- file.path(.path, "data", paste0(name, ".yaml"))

  # create empty oldrel data set if now oldrel exists
  if(identical(oldrel_path, "")) {
    oldrel_path <- create_empty_oldrel(.path = .path, name = name)
    cli::cli_alert_success("Created empty data set ('empty_gads.RDS') in data folder for initial diff.")
  }

  # yaml
  cli::cli_par()
  create_data_yaml(.path = .path, name = name, release_path = release_path, oldrel_path = oldrel_path,
                   version = version)
  cli::cli_alert_success("Created .yaml file in data folder.")

  # markdown changelog
  create_changelog(.path = .path, name = name, version = version)
  cli::cli_alert_success("Created changelog .md file.")

  # test file
  create_tests(.path = .path, name = name)
  cli::cli_alert_success("Created test template .R file.")

  # initialized test result icon (fail)
  create_initial_test_result(.path = .path, name = name)
  cli::cli_alert_success("Initialized test result icon.")
  cli::cli_end()

  # markdown entry (copy & paste?) for readem
  readme_snippet <- create_readme_snippet(name = name, version = version)
  clipr::write_clip(readme_snippet)
  cli::cli_alert("Pasted markdown entry for readme to clipboard. Please insert into table.")
  cli::cli_text(readme_snippet)
  # TODO: create markdown entry for readme; how to paste this to clipboard?

  invisible(return(readme_snippet))
}

create_empty_oldrel <- function(.path = getwd(), name) {
  # input validation

  #browser()
  empty_gads_path <- file.path(.path, "data/empty_gads.RDS")

  if(!file.exists(empty_gads_path)) {
    empty_gads_source <- system.file("extdata", "empty_gads.RDS", package = "eatDataTest")
    file.copy(from = empty_gads_source, to = empty_gads_path)
  }

  return(empty_gads_path)
}

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

  file_path <- file.path(.path, "tests", paste0("test-", name, ".R"))

  template_path <- system.file("templates", "test_eatGADS.R", package = "eatDataTest")
  template_string <- readLines(template_path, encoding = "UTF-8", warn = FALSE)

  data <- list(name = name)

  rendered_template <- strsplit(whisker::whisker.render(template_string, data), "\n")[[1]]

  writeLines(rendered_template, con = file_path)
  invisible(return())
}

create_initial_test_result <- function(.path = getwd(), name) {
  # input validation

  icon_result_path <- file.path(.path, "tests", paste0("result-", name, ".svg"))
  icon_source <- system.file("extdata", "fail_icon.svg", package = "eatDataTest")

  file.copy(from = icon_source, to = icon_result_path)
  invisible(return())
}

create_readme_snippet <- function(name, version = "v1.0") {
  # input validation

  md_content <- paste0("| ", name, "              | ", version, "    | ![s](tests/result-", name, ".svg) |")
  md_content
}


