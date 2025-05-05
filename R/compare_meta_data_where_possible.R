#' Compare meta data where possible.
#'
#' This function allows comparing the meta data of two data sets for all variables with identical names in both data sets.
#'
#' If `compare_meta_data_where_possible()` is used within data tests, it is highly recommended to test
#' `compared_variables` to ensure that tests are actually passing and not just lacking.
#'
#'@param data1 First `GADSdat` object.
#'@param data2 Second `GADSdat` object.
#'@param suffix Suffix that should be removed from all
#'@param ID_var Should paths in `release` and `oldrel` be converted to absolute paths?
#'
#'@return Returns a named list via `eatFDZ::compare_data()`:
#'* `compared_variables`: Which variables have been compared?
#'* `differences_variable_labels`: Differences on variable labels.
#'* `differences_value_labels`: Differences on value labels and missing tags.
#'
#'@examples
#'## tbd
#'
#'@export
compare_meta_data_where_possible <- function(data1, data2, suffix = NULL, ID_var) {
  # input validation


  names1 <- eatGADS::namesGADS(data1)
  names2 <- eatGADS::namesGADS(data2)
  # remove suffix where necessary
  if (!is.null(suffix)) {
    names1_without_suffix <- gsub(suffix, "", names1)
    if(any(names1 != names1_without_suffix)) {
      data1 <- eatGADS::changeVarNames(data1, oldNames = names1, newNames = names1_without_suffix)
    }

    names2_without_suffix <- gsub(suffix, "", names2)
    if(any(names2 != names2_without_suffix)) {
      data2 <- eatGADS::changeVarNames(data2, oldNames = names2, newNames = names2_without_suffix)
    }
  }

  common_names <- intersect(eatGADS::namesGADS(data1), eatGADS::namesGADS(data2))
  if(length(setdiff(common_names, ID_var)) < 1) {
    stop("No variables to compare.")
  }

  suppressMessages(data1_sub <- eatGADS::extractVars(data1, vars = common_names))
  suppressMessages(data2_sub <- eatGADS::extractVars(data2, vars = common_names))


  meta_comparison <- eatFDZ::compare_data(data1 = data1_sub,
                                          data2 = data2_sub,
                                          ID_var = ID_var,
                                          name_data1 = "data1",
                                          name_data2 = "data2")

  meta_comparison[["compared_variables"]] <- common_names

  meta_comparison[c("compared_variables", "differences_variable_labels", "differences_value_labels")]
}
