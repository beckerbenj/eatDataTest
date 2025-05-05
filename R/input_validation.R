# Input validation helpers for eatDataTest

validate_path <- function(.path){
  # checks that the path exists and is a directory, access = "r" ensures it’s readable
  checkmate::assert_directory_exists(.path, access = "r")
}
