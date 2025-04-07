#' Run Reverse Depedency Tests.
#'
#' Run all reverse dependency tests for a data set.
#'
#' If a project contains multiple data sets, these can sometimes depend on each other.
#' For example, a multiple imputaions data may depend on the unimputed data set.
#' `run_revdep_tests()` borrows the idea of reverse dependency tests from the R package
#' `revdep` and applies it do data sets.
#'
#' For instance, if a project contains three data sets (A, B, C). Both data sets B and C
#' depend on data set A. If `run_revdep_tests()` is run for data set A,
#' data tests will be performed for data sets B and C.
#'
#'@param .path Path to the `eatDataTest` repository. Defaults to the current working directory.
#'@param name Name of the data set.
#'@param overwrite_icons Should test result icons of the reverse dependenies be overwritten?.
#'
#'@return Returns a test report.
#'
#'@examples
#'## tbd
#'
#'@export
run_revdep_tests <- function(.path = getwd(), name, overwrite_icons = FALSE) {
  # input validation

  all_data_names <- get_all_data_names(.path)
  all_other_names <- setdiff(all_data_names, name)

  for(single_data_name in all_other_names) {
    #browser()
    depends <- get_depends(.path = .path, name = single_data_name)
    if(is.null(depends)) {
      next
    }
    depends_list <- strsplit(depends, split = ", ")[[1]]
    test_counter <- 0

    if(name %in% depends_list) {
      message("\nRunning tests for ", single_data_name, "\n")
      run_data_tests(.path = .path, name = single_data_name, overwrite_icon = overwrite_icons)
      test_counter <- test_counter + 1
    }
  }

  if(test_counter == 0) {
    message("No reverse dependencies found.")
  }

  invisible(NULL)
}

get_all_data_names <- function(.path = getwd()) {
  adapted_path <- adapt_path_for_yaml_reading(.path)
  data_path <- file.path(adapted_path, "data")

  data_files <- list.files(data_path, pattern = ".yaml", ignore.case = TRUE)
  gsub(".yaml", "", data_files)
}
