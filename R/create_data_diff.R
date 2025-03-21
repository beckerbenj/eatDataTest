#' Create Data and Meta Data Diff.
#'
#' Create a data diff that contains a list of differing variables and cross tables
#' for each differing variable as well as a meta data diff.
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
create_diff <- function(.path = getwd(), name, ID_var) {
  # input validation
  if(is.null(get_oldrel_path(.path = .path, name = name))) {
    stop("No oldrel path specified. No meaningful diff can be computed.")
  }

  meta_path <- file.path(.path, "diff", paste0(name, "_meta_diff.xlsx"))
  data_path <- file.path(.path, "diff", paste0(name, "_data_diff.xlsx"))

  release_data <- import_data(.path = .path, name = name, data_version = "release")
  oldrel_data <- import_data(.path = .path, name = name, data_version = "oldrel")

  meta_comparison <- eatFDZ::compare_data(data1 = release_data,
                                          data2 = oldrel_data,
                                          ID_var = ID_var,
                                          name_data1 = "release",
                                          name_data2 = "oldrel")

  data_comparison <- compare_actual_data(data1 = release_data,
                                          data2 = oldrel_data,
                                          ID_var = ID_var,
                                          name_data1 = "release",
                                          name_data2 = "oldrel")


  overwritexl(data_comparison, path = data_path)
  overwritexl(meta_comparison, path = meta_path)

  invisible(return())
}


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


  ## general overview
  # ----------------------------------------------------------
  diff_overview <- unique(data2$labels[data2$labels$varName %in% eatGADS_comparison$data_differences,
                                       c("varName", "varLabel")])


  ## specific cross tables
  # ----------------------------------------------------------
  out_list_var <- lapply(eatGADS_comparison$data_differences, function(nam) {
    out <- inspectDifferences2(data1, other_GADSdat = data2, varName = nam, id = ID_var)
    #browser()
    names(dimnames(out)) <- c(name_data1, name_data2)
    out_df <- as.data.frame.matrix(out)
    data.frame(rownames(out_df), out_df)
  })

  out <- vector(mode = "list", length = length(out_list_var) + 1)
  out[[1]] <- diff_overview
  if(length(diff_overview) > 0) {
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
  eatGADS:::check_GADSdat(GADSdat)
  eatGADS:::check_GADSdat(other_GADSdat)
  eatGADS:::check_characterArgument(varName, argName = "varName")
  eatGADS:::check_characterArgument(other_varName, argName = "other_varName")
  eatGADS:::check_characterArgument(id, argName = "id")
  eatGADS:::check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName", GADSdatName = "GADSdat")
  eatGADS:::check_vars_in_GADSdat(other_GADSdat, vars = other_varName, argName = "other_varName", GADSdatName = "other_GADSdat")
  eatGADS:::check_vars_in_GADSdat(GADSdat, vars = id, argName = "id", GADSdatName = "GADSdat")
  eatGADS:::check_vars_in_GADSdat(other_GADSdat, vars = id, argName = "id", GADSdatName = "other_GADSdat")

  #if(nrow(GADSdat$dat) != nrow(other_GADSdat$dat)) stop("'GADSdat' and 'other_GADSdat' have different row numbers.")
  if(any(is.na(GADSdat$dat[, id]))) stop("Missing values in 'id' column of 'GADSdat'.")
  if(any(is.na(other_GADSdat$dat[, id]))) stop("Missing values in 'id' column of 'other_GADSdat'.")
  #if(any(GADSdat$dat[, id] != other_GADSdat$dat[, id])) stop("'id' column is not equal for 'GADSdat' and 'other_GADSdat'.")

  if(is.numeric(GADSdat$dat[, varName]) && !is.numeric(other_GADSdat$dat[, other_varName])) {
    stop("'varName' column is numeric in 'GADSdat' but 'other_varName' is not numeric in 'other_GADSdat'.")
  }
  if(!is.numeric(GADSdat$dat[, varName]) && is.numeric(other_GADSdat$dat[, other_varName])) {
    stop("'other_varName' column is numeric in 'other_GADSdat' but 'varName' is not numeric in 'GADSdat'.")
  }

  if(isTRUE(all.equal(GADSdat$dat[, varName], other_GADSdat$dat[, other_varName], scale = 1))) {
    return("all.equal")
  }

  #unequal_rows <- c(which(other_GADSdat$dat[, other_varName] != GADSdat$dat[, varName]),
  #                  which(is.na(other_GADSdat$dat[, other_varName]) & !is.na(GADSdat$dat[, varName])),
  #                  which(!is.na(other_GADSdat$dat[, other_varName]) & is.na(GADSdat$dat[, varName])))
  #unequal_case_dat2 <- other_GADSdat$dat[unequal_rows, ]
  #unequal_case_dat1 <- GADSdat$dat[unequal_rows, ]

  # naming for cross_table
  nam_dnn <- c(varName, other_varName)
  if(identical(varName, other_varName)) {
    nam_dnn <- c("GADSdat", "other_GADSdat")
  }

  merged_df <- merge(GADSdat$dat[, c(id, varName)], other_GADSdat$dat[, c(id, other_varName)],
                     by = id, all = TRUE)

  table(merged_df[, length(id) + 1], merged_df[, length(id) + 2], useNA = "if",
        dnn = nam_dnn)

  #list(cross_table = table(GADSdat$dat[, varName], other_GADSdat$dat[, other_varName], useNA = "if",
  #                         dnn = nam_dnn),
  #     unequal_IDs = unequal_case_dat2[, id]
  #)
}

