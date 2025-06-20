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


validate_data_name <- function(name, path = getwd(), must_exist = FALSE) {

  # type string, no NA, at least one character
  checkmate::assert_string(name, min.chars = 1)

  # only allow letters, digits or underscores
  if (!grepl("^[a-zA-Z0-9_]+$", name)) {
    stop("Name must only contain letters, digits, or underscores.")
  }

  # Construct expected file paths
  expected_files <- stats::setNames(
    c( # for file.exists()
      file.path(path, "data",        paste0(name, ".yaml")),
      file.path(path, "changelogs",  paste0(name, ".md")),
      file.path(path, "tests",       paste0("result-", name, ".svg")),
      file.path(path, "tests",       paste0("test-", name, ".R"))
    ),
    c( # for user friendly error message
      file.path("data",        paste0(name, ".yaml")),
      file.path("changelogs",  paste0(name, ".md")),
      file.path("tests",       paste0("result-", name, ".svg")),
      file.path("tests",       paste0("test-", name, ".R"))
    )
  )

  file_status <- vapply(expected_files, file.exists, logical(1))

  if (must_exist) {
    # check existing data -> all files must exist
    if (!all(file_status)) {
      missing <- names(expected_files)[!file_status]
      stop("Data name '", name, "' is incomplete. Missing file(s): ",
           paste(missing, collapse = ", "), ".")
    }
  } else {
    # new data -> all files must NOT exist
    if (any(file_status)) {
      existing <- names(expected_files)[file_status]
      stop("Cannot use name '", name, "'. The following file(s) already exist: ",
           paste(existing, collapse = ", "), ".")
    }
  }
}

validate_depends <- function(depends, path = getwd()) {

  # type string, no NA, at least one character
  checkmate::assertString(depends)

  # Split into individual names
  depends_list <- strsplit(depends, split = ",\\s*")[[1]]

  # check if the splitting worked properly
  if (length(depends_list) == 0 || any(depends_list == "")) {
    stop("depends string must contain valid dataset names separated by commas (e.g., 'dataset1, dataset2').")
  }

  # check existence of the individual files
  yaml_files <- file.path("data", paste0(depends_list, ".yaml"))
  full_paths <- file.path(path, yaml_files)
  exists <- file.exists(full_paths)

  if (!all(exists)) {
    missing <- yaml_files[!exists]
    stop(sprintf("Missing YAML file(s) for dependency: %s", paste(missing, collapse = ", ")))
  }
}



# TO DO
# moved this here from read_data_yaml
validate_yaml <- function(.path = getwd(), name) {
  #yaml_file <- file.path(.path, "data", paste0(dep, ".yaml"))
}


