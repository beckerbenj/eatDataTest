adapt_path_for_yaml_reading <- function(.path){
  #browser()
  #stop(".path: ", .path, " and getwd: ", getwd())
  # For testing eatDataTest with testthat
  if (testthat::is_testing() && !isTRUE(getOption("testthat_interactive")) ||
      pkgload::is_loading() ||
      is_cmd_checking()) {
    if ("tests" %in% list.files(.path)) {
      return(.path)
    }
  }
  # For run_data_tests use cases
  if (testthat::is_testing() && !isTRUE(getOption("testthat_interactive"))) {
    if (!basename(.path) == "tests") {
      stop("test_data_path() must be called either via run_dat_tests() or interactively within a test file with the working directory properly set!")
    }
    return(dirname(.path))
  }
  #else if (pkgload::is_loading()) {
  #  if (!basename(.path) == "tests") {
  #    stop("test_data_path() must be called either via run_dat_tests() or interactively within a test file with the working directory properly set!")
   # }
  #  return(out <- dirname(.path))
  #}
  # For interactive use cases
  else {
    return(.path)
  }
}

is_cmd_checking <- function() {
  !is.na(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA))
}
