# Input validation helpers for eatDataTest

validate_path <- function(.path){
  # checks that the path exists and is a directory, access = "r" ensures it’s readable
  checkmate::assert_directory_exists(.path, access = "r")
}

validate_version <- function(version) {
  # version is a string
  checkmate::assert_string(version)

  # starts with "v", then one or more digits, then a dot, then one or more digits
  coll <- checkmate::makeAssertCollection()
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


validate_data_path <- function(path){
  file_extension <- tolower(tools::file_ext(path))
  allowed_extensions <- c("rds", "sav")
  checkmate::assert_choice(file_extension, allowed_extensions, .var.name = "file extension of path")

  # checks that the file exists, access = "r" ensures it’s readable
  checkmate::assert_file_exists(path, access = "r")
}


