#' Create Data and Meta Data Diff.
#'
#' `create_meta_diff` creates a list of differing variables as well as lists of meta differences
#' on variable and on value level.
#' `create_data_diff` create a list of differing variables and a cross tables
#' for each differing variable.
#' `create_diff` calls both functions.
#'
#'@param .path Path to the `eatDataTest` repository. Defaults to the current working directory.
#'@param name Name of the data set. The file will be named accordingly.
#'@param id Name of the id variable(s) in both data set.
#'@param tolerance A numeric value greater than or equal to 0. Differences smaller than `tolerance` are not reported. The default value is close to 1.5e-8.
#'
#'@return Creates two `.xlsx` files. Returns `NULL`.
#'
#'@examples
#'## tbd
#'
#'@export
create_diff <- function(.path = getwd(), name, id, tolerance = sqrt(.Machine$double.eps)) {
  create_meta_diff(.path = .path, name = name)
  create_data_diff(.path = .path, name = name, id = id, tolerance = tolerance)

  invisible(return())
}

#'@rdname create_diff
#'@export
create_meta_diff <- function(.path = getwd(), name) {
  # input validation
  if(is.null(get_oldrel_path(.path = .path, name = name))) {
    stop("No oldrel path specified. No meaningful diff can be computed.")
  }

  meta_path <- file.path(.path, "diff", paste0(name, "_meta_diff.xlsx"))

  release_data <- import_data(.path = .path, name = name, data_version = "release")
  oldrel_data <- import_data(.path = .path, name = name, data_version = "oldrel")

  meta_comparison <- eatFDZ::compare_data(data1 = release_data,
                                          data2 = oldrel_data,
                                          name_data1 = "release",
                                          name_data2 = "oldrel")

  overwritexl(meta_comparison, path = meta_path)

  invisible(return())
}

#'@rdname create_diff
#'@export
create_data_diff <- function(.path = getwd(), name, id, tolerance = sqrt(.Machine$double.eps)) {
  # input validation
  if(is.null(get_oldrel_path(.path = .path, name = name))) {
    stop("No oldrel path specified. No meaningful diff can be computed.")
  }

  data_path <- file.path(.path, "diff", paste0(name, "_data_diff.xlsx"))

  release_data <- import_data(.path = .path, name = name, data_version = "release")
  oldrel_data <- import_data(.path = .path, name = name, data_version = "oldrel")

  data_comparison <- compare_actual_data(data1 = release_data,
                                         data2 = oldrel_data,
                                         name_data1 = "release",
                                         name_data2 = "oldrel",
                                         id = id,
                                         tolerance = tolerance)


  overwritexl(data_comparison, path = data_path)

  invisible(return())
}


compare_actual_data <- function(data1, data2, name_data1 = "data1", name_data2 = "data2",
                                id, tolerance = sqrt(.Machine$double.eps)) {
  ## input validation
  # ----------------------------------------------------------
  eatGADS:::check_GADSdat(data1)
  eatGADS:::check_GADSdat(data2)

  eatGADS:::check_characterArgument(name_data1, argName = "name_data1")
  eatGADS:::check_characterArgument(name_data2, argName = "name_data2")

  eatGADS:::check_vars_in_GADSdat(data1, vars = id, argName = "id", GADSdatName = "data1")
  eatGADS:::check_vars_in_GADSdat(data2, vars = id, argName = "id", GADSdatName = "data2")

  ## initiate comparison
  # ----------------------------------------------------------
  eatGADS_comparison <- eatGADS::equalData(data1, data2, id = id, tolerance = tolerance)


  ## general overview
  # ----------------------------------------------------------
  diff_overview <- unique(data2$labels[data2$labels$varName %in% eatGADS_comparison$data_differences,
                                       c("varName", "varLabel")])


  ## specific cross tables
  # ----------------------------------------------------------
  out_list_var <- lapply(eatGADS_comparison$data_differences, function(nam) {
    inspectDifferences2(data1, other_GADSdat = data2, varName = nam, id = id)
    #out <- inspectDifferences2(data1, other_GADSdat = data2, varName = nam, id = ID_var)
    #browser()
    #names(dimnames(out)) <- c(name_data1, name_data2)
    #out_df <- as.data.frame.matrix(out)
    #data.frame(rownames(out_df), out_df)
  })

  out <- vector(mode = "list", length = length(out_list_var) + 1)
  out[[1]] <- diff_overview
  if(nrow(diff_overview) > 0) {
    out[2:(length(out_list_var) + 1)] <- out_list_var
  }

  names(out) <- c("vars_with_differences", eatGADS_comparison$data_differences)
  out
}

overwritexl <- function(x, path){
  if(file.exists(path)) {
    message("Overwriting existing path: ", path)
    unlink(path)
  }

  writexl::write_xlsx(x, path = path)
}

inspectDifferences2 <- function(GADSdat, varName, other_GADSdat = GADSdat, other_varName = varName, id) {
  if(isTRUE(all.equal(GADSdat$dat[, varName], other_GADSdat$dat[, other_varName], scale = 1))) {
    return("all.equal")
  }

  # naming for cross_table
  nam_dnn <- c(varName, other_varName)
  if(identical(varName, other_varName)) {
    nam_dnn <- c("release", "oldrel") # could/should this be more generic?
  }

  #browser()

  cli::cli_alert_info(paste0("Comparing ", varName))
  merged_df <- merge(GADSdat$dat[, c(id, varName)], other_GADSdat$dat[, c(id, other_varName)],
                     by = id, all = TRUE)

  position_varname <- length(id) + 1
  position_other_varname <- length(id) + 2

  merged_df[, position_varname] <- ifelse(is.na(merged_df[, position_varname]), "NA", merged_df[, position_varname])
  merged_df[, position_other_varname] <- ifelse(is.na(merged_df[, position_other_varname]), "NA", merged_df[, position_other_varname])

  # comparing again, as merging/ordering may have solved the issue
  if(isTRUE(all.equal(merged_df[, position_varname], merged_df[, position_other_varname], scale = 1))) {
    return("all.equal")
  }

  rows_with_differences <- merged_df[, position_varname] != merged_df[, position_other_varname]


  cross_table_as_data_frame(merged_df[rows_with_differences, position_varname],
                            merged_df[rows_with_differences, position_other_varname], useNA = "ifany",
                            name1 = nam_dnn[1], name2 = nam_dnn[2])

}

cross_table_as_data_frame <- function(v1, v2, name1, name2, useNA = "ifany") {
  # input validation

  tab <- table(v1, v2, useNA = useNA)
  m1 <- matrix(tab, ncol = ncol(tab))

  df_no_colnames <- data.frame(c1 = rep(NA, length(dimnames(tab)[[1]])), # name of variable/data.frame
                               c2 = dimnames(tab)[[1]], # values
                               m1) # actual table
  out <- rbind(c(NA, name2, rep(NA, length(dimnames(tab)[[2]]))), # name of variable/data.frame
          c(name1, NA, dimnames(tab)[[2]]), # values
          df_no_colnames)
  colnames(out) <- NULL
  out

}

