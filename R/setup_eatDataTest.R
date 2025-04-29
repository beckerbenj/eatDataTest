#' Setup `eatDataTest`.
#'
#' Add the infrastructure for using the directory as a`eatDataTest` repository. This includes
#' * a `data` folder
#' * a `changelogs` folder
#' * a `tests` folder
#' * a `diff` folder
#' * a `Readme.md` containing the following elements: a table to list all data sets and
#' instructions on how to use the repository for data contributers.
#'
#'@param .path Path to the `eatDataTest` repository. Defaults to the current working directory.
#'
#'@return Creates repository infrastructure. Returns a markdown snippet.
#'
#'@examples
#'## tbd
#'
#'@export
setup_eatDataTest <- function(.path = getwd()) {
  # input validation

  cli::cli_par()
  create_directory(.path, name = "data")
  create_directory(.path, name = "changelogs")
  create_directory(.path, name = "tests")
  create_directory(.path, name = "diff")

  # markdown entry (copy & paste?) for readem
  readme_snippet <- load_readme_setup()
  clipr::write_clip(readme_snippet)
  cli::cli_alert("Pasted markdown entry for readme to clipboard. Please insert into Readme.md.")
  cli::cli_text(readme_snippet)

  invisible(return(readme_snippet))
  #invisible(return())
}

create_directory <- function(.path = getwd(), name) {
  if(dir.exists(file.path(.path, name))) {
    cli::cli_alert_info(paste0(name, " folder already exists."))
  } else {
    dir.create(file.path(.path, name))
    cli::cli_alert_success(paste0("Created ", name, " folder."))
  }
  invisible(return())
}


load_readme_setup <- function() {
  template_path <- system.file("templates", "README.md", package = "eatDataTest")
  readLines(template_path, encoding = "UTF-8", warn = FALSE)
}
