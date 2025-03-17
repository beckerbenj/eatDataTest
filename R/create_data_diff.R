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

  #stop("is_checking:", is_checking(), " and path: ", .path)

  release_data <- import_data(.path = .path, name = name, data_version = "release")
  oldrel_data <- import_data(.path = .path, name = name, data_version = "oldrel")

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


## TODO: Refactor comparison functions in eatGADS (equalGADS, compareGADS, inspectData, inspectMetaData)
# and eatFDZ (compare_data)
# Probably everything should live in eatGADS? Clean and fast equalGADS, precies inspect and
# extensive reporting in compare_data and compare_meta_data?

compare_actual_data <- function(data1, data2, name_data1 = "data1", name_data2 = "data2", ID_var) {
  ## input validation
  # ----------------------------------------------------------
  eatGADS:::check_GADSdat(data1)
  eatGADS:::check_GADSdat(data2)

  eatGADS:::check_characterArgument(ID_var, argName = "ID_var")
  eatGADS:::check_characterArgument(name_data1, argName = "name_data1")
  eatGADS:::check_characterArgument(name_data2, argName = "name_data2")

  eatGADS:::check_vars_in_GADSdat(data1, vars = ID_var, argName = "ID_var", GADSdatName = "data1")
  eatGADS:::check_vars_in_GADSdat(data2, vars = ID_var, argName = "ID_var", GADSdatName = "data2")

  ## initiate comparison
  # ----------------------------------------------------------
  eatGADS_comparison <- eatGADS::equalGADS(data1, data2, id = ID_var)


  ## compare data
  # ----------------------------------------------------------
  out_list_var <- lapply(eatGADS_comparison$data_differences, function(nam) {
    out <- eatGADS::inspectDifferences(data1, other_GADSdat = data2, varName = nam, id = ID_var)
    #browser()
    names(dimnames(out$cross_table)) <- c(name_data1, name_data2)
    out$cross_table
  })

  names(out_list_var) <- eatGADS_comparison$data_differences

  #browser()

  ## TODO:
  # Overview as first list element. Just a list of variables that differ in their data (maybe with variable labels?)
  # Then a variable number of additional sheets, each containing just the cross table
  # => check eatFDZ compare_data() for code ideas

}
