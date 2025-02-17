#' Create Data Diff.
#'
#' Create a data diff that contains ...
#'
#'@param .path Path to the `eatDaT` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'@param ID_var Name of the id variable in bot data set.
#'
#'@return Creates a .xlsx. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
create_data_diff <- function(.path = getwd(), name, ID_var) {
  # input validation

  out_path <- file.path(.path, "diff", paste0(name, "_diff.xlsx"))

  data_yaml <- read_data_yaml(.path = .path, name = name)
  release_data <- eatGADS::import_spss(data_yaml$release)
  oldrel_data <- eatGADS::import_spss(data_yaml$oldrel)

  out_compare <- eatFDZ::compare_data(data1 = release_data,
                       data2 = oldrel_data,
                       ID_var = ID_var,
                       name_data1 = "release",
                       name_data2 = "oldrel")

  if(file.exists(out_path)) {
    message("Overwriting existing diff.")
    unlink(out_path)
  }

  writexl::write_xlsx(out_compare, path = out_path)
  invisible(return())
}
