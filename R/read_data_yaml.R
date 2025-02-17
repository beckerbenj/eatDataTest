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

  file_path <- file.path(.path, "data", paste0(name, ".yaml"))

  yaml::yaml.load_file(file_path)
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
