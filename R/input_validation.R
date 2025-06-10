# Input validation helpers for eatDataTest

validate_directory_path <- function(path){

  # path needs to be passed as a string
  checkmate::assert_string(path)

  # checks that the path exists and is a directory, access = "r" ensures it’s readable
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
  coll <- checkmate::makeAssertCollection() # for custom error messages
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

  # path needs to be passed as a string
  checkmate::assert_string(path)

  # the file exists, access = "r" ensures it’s readable
  checkmate::assert_file_exists(path, access = "r", .var.name = argName)

  # must have the correct file extension: "rds" or "sav"
  file_extension <- tolower(tools::file_ext(path))
  allowed_extensions <- c("rds", "sav")
  checkmate::assert_choice(file_extension, allowed_extensions, .var.name = paste0("file extension of ", argName))
}


validate_new_data_name <- function(name) { # 'validate_name' already exists in read_data_yaml.R

  # type string, no NA, at least one character
  checkmate::assert_string(name, min.chars = 1)

  # only allow letters, digits or underscores
  coll <- checkmate::makeAssertCollection()
  if (!grepl("^[a-zA-Z0-9_]+$", name)) {
      coll$push("Name must only contain letters, digits, or underscores.")
  }
  checkmate::reportAssertions(coll)
}


validate_depends <- function(path = getwd(), depends) {

  # type string, no NA, at least one character
  checkmate::assertString(depends)

  # Split into individual names
  depends_list <- strsplit(depends, split = ",\\s*")[[1]]

  # check if the splitting worked properly
  coll <- checkmate::makeAssertCollection()
  if (length(depends_list) == 0 || any(depends_list == "")) {
    coll$push("depends string must contain valid dataset names separated by commas (e.g., 'dataset1, dataset2').")
  } else { # if splitting worked, continue more checks

    # check existence of the individual files
    for (dep in depends_list) {
      yaml_file <- file.path(path, "data", paste0(dep, ".yaml"))
      if (!file.exists(yaml_file)) {
        coll$push(sprintf("Missing YAML file for dependency: '%s' (%s)", dep, yaml_file))
      }
    }

  }
  checkmate::reportAssertions(coll)
}


# TO DO
# moved this here from read_data_yaml
validate_yaml <- function(.path = getwd(), name) {
  #yaml_file <- file.path(.path, "data", paste0(dep, ".yaml"))
}


