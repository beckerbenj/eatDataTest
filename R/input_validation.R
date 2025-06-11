# Input validation helpers for eatDataTest

validate_directory_path <- function(path){

  # path needs to be passed as a string
  checkmate::assert_string(path)

  # checks that the path exists and is a directory, access = "r" ensures it is readable
  checkmate::assert_directory_exists(path, access = "r")

  expected_subdirs <- c("data", "changelogs", "tests", "diff")
  actual_subdirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  missing_subdirs <- setdiff(expected_subdirs, actual_subdirs)

  if (length(missing_subdirs) > 0) {
    stop("The following required subdirectories are missing from '", path, "': ",
        paste(missing_subdirs, collapse = ", "), ". Try running `setup_eatDataTest()` to create them."
    )
  }
}


validate_version <- function(version) {

  # version is a string
  checkmate::assert_string(version)

  # starts with "v", then one or more digits, then a dot, then one or more digits
  if(!grepl("^v", version)){
    stop("Version must start with a lowercase 'v' (e.g. 'v1.0').")
  }
  if(!grepl("^v\\d+", version)){
    stop("Version must have one or more digits immediately after 'v' (e.g. 'v1.0').")
  }
  if(!grepl("^v\\d+\\.", version)){
    stop("Version must include a dot after the digits (e.g. 'v1.0').")
  }
  if(!grepl("^v\\d+\\..+", version)){
    stop("Version must have at least one character after the dot (e.g. 'v1.0').")
  }
}


validate_data_path <- function(path, argName = "path"){

  # path needs to be passed as a string
  checkmate::assert_string(path)

  # the file exists, access = "r" ensures it is readable
  checkmate::assert_file_exists(path, access = "r", .var.name = argName)

  # must have the correct file extension: "rds" or "sav"
  file_extension <- tolower(tools::file_ext(path))
  allowed_extensions <- c("rds", "sav")
  checkmate::assert_choice(file_extension, allowed_extensions, .var.name = paste0("file extension of ", argName))
}


validate_new_data_name <- function(name, .path = getwd()) {

  # type string, no NA, at least one character
  checkmate::assert_string(name, min.chars = 1)

  # only allow letters, digits or underscores
  if (!grepl("^[a-zA-Z0-9_]+$", name)) {
      stop("Name must only contain letters, digits, or underscores.")
  }

  # Check that the file doesn't already exist:
  # Construct expected file paths
  data_file      <- file.path(.path, "data", paste0(name, ".yaml"))
  changelog_file <- file.path(.path, "changelogs", paste0(name, ".md"))
  result_file    <- file.path(.path, "tests", paste0("result-", name, ".svg"))
  test_file      <- file.path(.path, "tests", paste0("test-", name, ".R"))

  # which of those already exist?
  existing_files <- c(
    if (file.exists(data_file))      {file.path("data", paste0(name, ".yaml"))},
    if (file.exists(changelog_file)) {file.path("changelogs", paste0(name, ".md"))},
    if (file.exists(result_file))    {file.path("tests", paste0("result-", name, ".svg"))},
    if (file.exists(test_file))      {file.path("tests", paste0("test-", name, ".R"))}
  )

  # if any already exist, we cannot use the provided name
  if (length(existing_files) > 0) {
    stop("Cannot use name '", name, "'. The following file(s) already exist in ", .path, ": ", paste(existing_files, collapse = ", "), ".")
  }
}


validate_depends <- function(path = getwd(), depends) {

  # type string, no NA, at least one character
  checkmate::assertString(depends)

  # Split into individual names
  depends_list <- strsplit(depends, split = ",\\s*")[[1]]

  # check if the splitting worked properly
  if (length(depends_list) == 0 || any(depends_list == "")) {
    stop("depends string must contain valid dataset names separated by commas (e.g., 'dataset1, dataset2').")
  }

    # check existence of the individual files
  for (dep in depends_list) {
    yaml_file <- file.path(path, "data", paste0(dep, ".yaml"))
    if (!file.exists(yaml_file)) {
      stop(sprintf("Missing YAML file for dependency: '%s' (%s)", dep, yaml_file))
    }
  }
}



# TO DO
# moved this here from read_data_yaml
validate_yaml <- function(.path = getwd(), name) {
  #yaml_file <- file.path(.path, "data", paste0(dep, ".yaml"))
}


