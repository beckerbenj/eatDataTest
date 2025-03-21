#' Import Data.
#'
#' Imports a data set based on its yaml file. Currently, `import_data()` supports
#' `.RDS` and `.sav` files.
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'@param data_version Available options are `'release'` and `'oldrel'`. Defaults to `'release'`.
#'
#'@return Returns a data set in `GADSdat` format.
#'
#'@examples
#'## tbd
#'
#'@export
import_data <- function(.path = getwd(), name, data_version = c("release", "oldrel")) {
  # input validation

  data_version <- match.arg(data_version)

  file_path <- read_data_yaml(.path = .path, name = name)[[data_version]]
  file_extension <- tools::file_ext(file_path)

  if(is.null(file_path)) {
    stop("No path specified for ", name, " in version ", data_version, ". Please check data/", name, ".yaml")
  }
  #browser()

  if(identical(file_extension, "sav")) {
    gads <- eatGADS::import_spss(file_path)
  } else if(file_extension %in% c("RDS", "Rds", "rds")) {
    gads <- readRDS(file_path)
  } else {
    stop("Unsupported file extension: ", file_extension)
  }

  eatGADS:::check_GADSdat(gads)

  gads
}
