#' Read Data Yaml.
#'
#' Reads a data yaml that contains paths to the current data version (`'release_path'`),
#' the previous version (`'oldrel_path'`)
#' and the current version number (`'version'`).
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'
#'@return Returns a named list.
#'
#'@examples
#'## tbd
#'
#'@export
read_data_yaml <- function(.path = getwd(), name) {
  # input validation

  adapted_path <- adapt_path_for_yaml_reading(.path)
  file_path <- file.path(adapted_path, "data", paste0(name, ".yaml"))

  yaml_input <- yaml::yaml.load_file(file_path)

  #browser()
  # normalize path
  # TODO: Does this always work/make sense?
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(adapted_path)
  yaml_input$release <- tools::file_path_as_absolute(yaml_input$release)
  if(!is.null(yaml_input$oldrel)) {
    yaml_input$oldrel <- tools::file_path_as_absolute(yaml_input$oldrel)
  }
  yaml_input
}

get_release_path <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name)$release
}

get_oldrel_path <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name)$oldrel
}

get_version <- function(.path = getwd(), name) {
  read_data_yaml(.path = .path, name = name)$version
}
