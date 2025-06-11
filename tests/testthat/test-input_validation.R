
# Setup for tests --------------------------------------------------------------

# create temporary directory
test_dir <- file.path(tempdir(), "test_repo")
dir.create(test_dir)

# create subdirectories
changelog_dir <- file.path(test_dir, "changelogs")
dir.create(changelog_dir)
tests_dir <- file.path(test_dir, "tests")
dir.create(tests_dir)
data_dir <- file.path(test_dir, "data")
dir.create(data_dir)
diff_dir <- file.path(test_dir, "diff")
dir.create(diff_dir)

# a dataframe
example_data_path <- "helper_example_repo/data_files/example_data"

# save a random text file
writeLines("This is not a data frame.", file.path(test_dir, "not_a_dataframe.txt"))
example_text_file <- file.path(test_dir, "not_a_dataframe.txt")

# a non existent file
non_existent_file <- file.path(test_dir, "nonexistent_file.rds")
if(!file.exists(non_existent_file)){unlink(non_existent_file, recursive = TRUE)}



# validate_directory_path ------------------------------------------------------

test_that("validate_directory_path() checks valid strings", {
  expect_error(validate_directory_path(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_directory_path(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_directory_path(NA), "May not be NA.")
  expect_error(validate_directory_path(c("./path1", "./path2")), "Must have length 1.")
})


test_that("validate_directory_path() fails for non-existing directories", {
  non_existent_path <- tempfile()
  expect_error(validate_directory_path(non_existent_path), "does not exist")
})


test_that("validate_directory_path() checks subdierectories", {

  # validation test should pass because all directories exist
  expect_silent(validate_directory_path(test_dir))

  # validation test should fail if one subdirectory is missing

  unlink(changelog_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "missing.*changelogs")
  dir.create(changelog_dir)

  unlink(tests_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "missing.*tests")
  dir.create(tests_dir)

  unlink(data_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "missing.*data")
  dir.create(data_dir)

  unlink(diff_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "missing.*diff")

  unlink(changelog_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "missing.*changelogs, diff")
  dir.create(diff_dir)
  dir.create(changelog_dir)

  # test the behavior if the directory contains all required subdirectories plus others
  extra_dir <- file.path(test_dir, "extra")
  dir.create(extra_dir)
  expect_silent(validate_directory_path(test_dir))

})


# validate_version -------------------------------------------------------------

test_that("validate_version() accepts correct version format", {
  expect_silent(validate_version("v1.0"))
  expect_silent(validate_version("v123.456"))
  expect_silent(validate_version("v0.0"))
  expect_silent(validate_version("v001.099"))
  expect_silent(validate_version("v1.alpha"))
  expect_silent(validate_version("v1.beta1"))
  expect_silent(validate_version("v1.0.2"))

})

test_that("validate_version() fails if input is not a string", {
  expect_error(validate_version(1.0), "Must be of type 'string', not 'double'.")
  expect_error(validate_version(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_version(NA), "May not be NA.")
})

test_that("validate_version() requires version to start with 'v'", {
  expect_error(validate_version("1.0"), "must start with a lowercase 'v'")
  expect_error(validate_version(" v1.0"), "must start with a lowercase 'v'") # leading whitespace
})

test_that("validate_version() requires digits after 'v'", {
  expect_error(validate_version("v"), "have one or more digits immediately after")
  expect_error(validate_version("v."), "have one or more digits immediately after")
})

test_that("validate_version() requires a dot after digits", {
  expect_error(validate_version("v1"), "dot after the digits")
})

test_that("validate_version() requires something after the dot", {
  expect_error(validate_version("v1."), "must have at least one character after the dot")
})




# validate_data_path -----------------------------------------------------------

test_that("validate_data_path() accepts valid data", {
  expect_silent(validate_data_path(paste0(example_data_path, ".rds")))
  expect_silent(validate_data_path(paste0(example_data_path, ".sav")))
  # should accept uppercase and mixed spelling as well:
  expect_silent(validate_data_path(paste0(example_data_path, ".RDS")))
  expect_silent(validate_data_path(paste0(example_data_path, ".rDs")))
  expect_silent(validate_data_path(paste0(example_data_path, ".SAV")))
  expect_silent(validate_data_path(paste0(example_data_path, ".sAv")))
})

test_that("validate_data_path() fails if path is not a string", {
  expect_error(validate_data_path(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_data_path(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_data_path(NA), "May not be NA.")
})

test_that("validate_data_path() fails if file does not exist", {
  expect_error(validate_data_path(non_existent_file), "File does not exist")
})

test_that("validate_data_path() fails if file extension is not rds or sav", {
  expect_error(validate_data_path(example_text_file), "Must be element of set \\{'rds','sav'\\}, but is 'txt'")
})

test_that("validate_data_path() includes custom argName in error messages", {
  expect_error(validate_data_path(non_existent_file, "my_data"), "my_data")
  expect_error(validate_data_path(example_text_file, "my_data"), "my_data")
})



# validate_new_data_name -------------------------------------------------------

test_that("validate_new_data_name() accepts valid names", {
  expect_silent(validate_new_data_name("dataset1"))
  expect_silent(validate_new_data_name("Dataset_2025"))
  expect_silent(validate_new_data_name("X123"))
  expect_silent(validate_new_data_name("data_name"))
  expect_silent(validate_new_data_name("Data_Name_1"))
})

test_that("validate_new_data_name() rejects invalid characters", {
  expect_error(validate_new_data_name("data-name"), "only contain letters, digits, or underscores")
  expect_error(validate_new_data_name("data.name"), "only contain letters, digits, or underscores")
  expect_error(validate_new_data_name("data name"), "only contain letters, digits, or underscores")
  expect_error(validate_new_data_name("name!"), "only contain letters, digits, or underscores")
  expect_error(validate_new_data_name(" name"), "only contain letters, digits, or underscores")
})

test_that("validate_new_data_name() rejects non-string input", {
  expect_error(validate_new_data_name(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_new_data_name(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_new_data_name(NA), "May not be NA.")
})

test_that("validate_new_data_name() rejects empty string", {
  expect_error(validate_new_data_name(""), "must have at least 1 characters")
  expect_error(validate_new_data_name(" "), "only contain letters, digits, or underscores")
})

test_that("existing data name is rejected", {
  file.create(file.path(data_dir, "existing.yaml"))
  expect_error(validate_new_data_name("existing", .path = test_dir), "already exist.*existing.yaml")
  file.create(file.path(changelog_dir, "existing.md"))
  file.create(file.path(tests_dir, "result-existing.svg"))
  file.create(file.path(tests_dir, "test-existing.R"))
  expect_error(validate_new_data_name("existing", .path = test_dir), "already exist.*existing\\.yaml.*existing\\.md.*result-existing\\.svg.*test-existing\\.R")
})
