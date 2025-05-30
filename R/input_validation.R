# Input validation helpers for eatDataTest

validate_directory_path <- function(path){

  # checks that the path exists and is a directory, access = "r" ensures it’s readable
  checkmate::assert_directory_exists(path, access = "r", add = coll)

  # check that all necessary sub-directories exist
  # makeAssertCollection: collect all error messages
  # & allow custom error messages ("Try calling `setup_eatDataTest()` first.")
  coll <- checkmate::makeAssertCollection()
  expected_subdirs <- c("data", "changelogs", "tests", "diff")
  actual_subdirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)

  for (subdir in expected_subdirs) {
    if (!(subdir %in% actual_subdirs)) {
      coll$push(sprintf("Missing required subdirectory: '%s/' in '%s'. Try calling `setup_eatDataTest()` first.", subdir, path))
    }
  }
  checkmate::reportAssertions(coll)
}

validate_version <- function(version) {
  coll <- checkmate::makeAssertCollection()

  # version is a string
  checkmate::assert_string(version, add = coll)

  # starts with "v", then one or more digits, then a dot, then one or more digits
  if(!grepl("^v", version)){
    coll$push("Version must start with a lowercase 'v' (e.g. 'v1.0').")
  }
  else if(!grepl("^v\\d+", version)){
    coll$push("Version must have one or more digits immediately after 'v' (e.g. 'v1.0').")
  }
  else if(!grepl("^v\\d+\\.", version)){
    coll$push("Version must include a dot after the digits (e.g. 'v1.0').")
  }
  else if(!grepl("^v\\d+\\.\\d+$", version)){
    coll$push("Version must end with one or more digits after the dot (e.g. 'v1.0').")
  }
  checkmate::reportAssertions(coll)
}


validate_data_path <- function(path, argName = "path"){
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_string(path, add = coll)

  file_extension <- tolower(tools::file_ext(path))
  allowed_extensions <- c("rds", "sav")
  checkmate::assert_choice(file_extension, allowed_extensions, .var.name = paste0("file extension of ", argName), add = coll)

  # checks that the file exists, access = "r" ensures it’s readable
  checkmate::assert_file_exists(path, access = "r", .var.name = argName, add = coll)

  checkmate::reportAssertions(coll)
}



validate_new_data_name <- function(name) { # 'validate_name' already exists in read_data_yaml.R
  coll <- checkmate::makeAssertCollection()
  # type string, no NA, at least one character
  checkmate::assert_string(name, min.chars = 1, add = coll)
  # only allow letters, digits or underscores
  if (!grepl("^[a-zA-Z0-9_]+$", name)) {
      coll$push("Name must only contain letters, digits, or underscores.")
  }
  checkmate::reportAssertions(coll)
}


validate_depends <- function(.path = getwd(), depends) {
  coll <- checkmate::makeAssertCollection()

  # type string, no NA, at least one character
  checkmate::assertString(depends, add = coll)

  # Split into individual names
  depends_list <- strsplit(depends, split = ",\\s*")[[1]]

  # check if the splitting worked properly
  if (length(depends_list) == 0 || any(depends_list == "")) {
    coll$push("Depends string must contain valid dataset names separated by commas (e.g., 'dataset1, dataset2').")
  }

  # check existence of the individual files
  for (dep in depends_list) {
    yaml_file <- file.path(.path, "data", paste0(dep, ".yaml"))
    if (!file.exists(yaml_file)) {
      coll$push(sprintf("Missing YAML file for dependency: '%s' (%s)", dep, yaml_file))
    }
  }

  checkmate::reportAssertions(coll)
}


# TO DO
# moved this here from read_data_yaml
validate_yaml <- function(.path = getwd(), name) {
  yaml_file <- file.path(.path, "data", paste0(dep, ".yaml"))
}


