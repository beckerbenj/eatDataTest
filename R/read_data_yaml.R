#' Read Data Yaml.
#'
#' `read_data_yaml()` reads a data yaml that has been created via `create_data_yaml()`.
#' The respective get-functions are wrappers around `read_data_yaml()` and simply return a single character value.
#'
#'@param .path Path to the `eatDataTest` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'@param absolute_paths Should paths in `release` and `oldrel` be converted to absolute paths?
#'
#'@return Returns a named list with the following entries:
#'* `version`: Version number of the current data set.
#'* `release`: Path to the current data version.
#'* `oldrel`: Path to the previous data version.
#'* `depends`: What other data sets does this data set depend on? List of data set names separated by `", "`.
#'
#'@examples
#'## tbd
#'
#'@export
read_data_yaml <- function(.path = getwd(), name, absolute_paths = TRUE) {
  # input validation

  adapted_path <- adapt_path_for_yaml_reading(.path)
  file_path <- file.path(adapted_path, "data", paste0(name, ".yaml"))

  yaml_input <- yaml::yaml.load_file(file_path)

  if(absolute_paths) {
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    setwd(adapted_path)

    yaml_input$release <- tools::file_path_as_absolute(yaml_input$release)
    if(!is.null(yaml_input$oldrel)) {
      yaml_input$oldrel <- tools::file_path_as_absolute(yaml_input$oldrel)
    }
  }

  yaml_input
}

#' @rdname read_data_yaml
#' @export
get_release_path <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name, absolute_paths = TRUE)$release
}

#' @rdname read_data_yaml
#' @export
get_oldrel_path <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name, absolute_paths = TRUE)$oldrel
}

#' @rdname read_data_yaml
#' @export
get_version <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name, absolute_paths = FALSE)$version
}

#' @rdname read_data_yaml
#' @export
get_depends <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name, absolute_paths = FALSE)$depends
}
