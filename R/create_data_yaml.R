#' Create Data Yaml.
#'
#' Create a data yaml that contains paths to the current data version (`'release_path'`),
#' the previous version (`'oldrel_path'`)
#' and the current version number (`'version'`).
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'@param release_path Path to the data set.
#'@param oldrel_path Optional: Path to the previous data version. Defaults to `NULL`.
#'@param version Version number of the current data set. Defaults to `'v1.0'`.
#'
#'@return Creates a .yaml. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
create_data_yaml <- function(.path = getwd(), name, release_path, oldrel_path = NULL,
                             version = "v1.0", depends = NULL) {
  # input validation

  file_path <- file.path(.path, "data", paste0(name, ".yaml"))

  yaml_content <- list(version, release_path, oldrel_path, depends)
  names(yaml_content) <- c("version", "release", "oldrel", "depends")

  yaml::write_yaml(yaml_content, file = file_path)
  invisible(return())
}
