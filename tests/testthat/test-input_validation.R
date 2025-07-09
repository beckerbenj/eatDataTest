
# Setup for tests --------------------------------------------------------------

# create temporary directory
test_dir <- file.path(tempdir(), "test_repo_inputvalidation")
if(dir.exists(test_dir)){unlink(test_dir, recursive = TRUE)}
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
writeLines("This is not a data frame.", file.path(test_dir, "example_test_file.txt"))
example_text_file <- file.path(test_dir, "example_test_file.txt")

# a non existent file
non_existent_file <- file.path(test_dir, "nonexistent_file.rds")
if(file.exists(non_existent_file)){unlink(non_existent_file, recursive = TRUE)}



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

  # create new temporary directory to make this test self-contained
  temp_test_dir <- file.path(tempdir(), "temp_test_repo_inputvalidation")
  if(dir.exists(temp_test_dir)){unlink(temp_test_dir, recursive = TRUE)}
  dir.create(temp_test_dir)

  # create subdirectories
  dir.create(file.path(temp_test_dir, "changelogs"))
  dir.create(file.path(temp_test_dir, "tests"))
  dir.create(file.path(temp_test_dir, "data"))
  dir.create(file.path(temp_test_dir, "diff"))

  # validation test should pass because all directories exist
  expect_silent(validate_directory_path(temp_test_dir))

  # validation test should fail if one subdirectory is missing

  unlink(file.path(temp_test_dir, "changelogs"), recursive = TRUE)
  expect_error(validate_directory_path(temp_test_dir), "missing.*changelogs")
  dir.create(file.path(temp_test_dir, "changelogs"))

  unlink(file.path(temp_test_dir, "tests"), recursive = TRUE)
  expect_error(validate_directory_path(temp_test_dir), "missing.*tests")
  dir.create(file.path(temp_test_dir, "tests"))

  unlink(file.path(temp_test_dir, "data"), recursive = TRUE)
  expect_error(validate_directory_path(temp_test_dir), "missing.*data")
  dir.create(file.path(temp_test_dir, "data"))

  unlink(file.path(temp_test_dir, "diff"), recursive = TRUE)
  expect_error(validate_directory_path(temp_test_dir), "missing.*diff")

  unlink(file.path(temp_test_dir, "changelogs"), recursive = TRUE)
  expect_error(validate_directory_path(temp_test_dir), "missing.*changelogs, diff")
  dir.create(file.path(temp_test_dir, "diff"))
  dir.create(file.path(temp_test_dir, "changelogs"))

  # test the behavior if the directory contains all required subdirectories plus others
  dir.create(file.path(temp_test_dir, "extra"))
  expect_silent(validate_directory_path(file.path(temp_test_dir)))

  #clean-up
  unlink(temp_test_dir, recursive = TRUE)
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

  tempfiles <- file.path(test_dir,
                         c("example_data.rds",
                           "example_data.sav", # should accept uppercase and mixed spelling as well:
                           "example_data.RDS",
                           "example_data.rDs",
                           "example_data.SAV",
                           "example_data.sAv"))

  for (file in tempfiles){
    file.create(file)
    expect_silent(validate_data_path(file))
    unlink(file)
  }
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



# validate_data_name -------------------------------------------------------

test_that("validate_data_name() accepts valid names", {
  expect_silent(validate_data_name("dataset1"))
  expect_silent(validate_data_name("Dataset_2025"))
  expect_silent(validate_data_name("X123"))
  expect_silent(validate_data_name("data_name"))
  expect_silent(validate_data_name("Data_Name_1"))
})

test_that("validate_data_name() rejects invalid characters", {
  expect_error(validate_data_name("data-name"), "only contain letters, digits, or underscores")
  expect_error(validate_data_name("data.name"), "only contain letters, digits, or underscores")
  expect_error(validate_data_name("data name"), "only contain letters, digits, or underscores")
  expect_error(validate_data_name("name!"), "only contain letters, digits, or underscores")
  expect_error(validate_data_name(" name"), "only contain letters, digits, or underscores")
})

test_that("validate_data_name() rejects non-string input", {
  expect_error(validate_data_name(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_data_name(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_data_name(NA), "May not be NA.")
})

test_that("validate_data_name() rejects empty string", {
  expect_error(validate_data_name(""), "must have at least 1 characters")
  expect_error(validate_data_name(" "), "only contain letters, digits, or underscores")
})

test_that("validate_data_name() rejects existing name", {
  file.create(file.path(data_dir, "existing.yaml"))
  expect_error(validate_data_name("existing", path = test_dir), "already exist.*existing.yaml\\.")
  file.create(file.path(changelog_dir, "existing.md"))
  file.create(file.path(tests_dir, "result-existing.svg"))
  file.create(file.path(tests_dir, "test-existing.R"))
  expect_error(validate_data_name("existing", path = test_dir), "already exist.*existing\\.yaml.*existing\\.md.*result-existing\\.svg.*test-existing\\.R")
})

test_that("validate_data_name() passes when existing = TRUE and all files exist", {
  file.create(file.path(test_dir, "data", paste0("example", ".yaml")))
  file.create(file.path(test_dir, "changelogs", paste0("example", ".md")))
  file.create(file.path(test_dir, "tests", paste0("result-", "example", ".svg")))
  file.create(file.path(test_dir, "tests", paste0("test-", "example", ".R")))
  expect_silent(validate_data_name("example", path = test_dir, existing = TRUE))
})

test_that("validate_data_name() fails when existing = TRUE and some files are missing", {
  expect_error(validate_data_name("example2", path = test_dir, existing = TRUE), "incomplete.*data/example2\\.yaml.*changelogs/example2\\.md.*result-example2\\.svg.*test-example2\\.R")
  file.create(file.path(test_dir, "changelogs", paste0("example2", ".md")))
  file.create(file.path(test_dir, "tests", paste0("result-", "example2", ".svg")))
  expect_error(validate_data_name("example2", path = test_dir, existing = TRUE), "incomplete.*data/example2\\.yaml.*tests/test-example2\\.R.")
})


# validate_depends -------------------------------------------------------------

test_that("validate_depends() passes when all YAML files exist", {
  expect_silent(validate_depends(path = "./helper_example_repo", depends = "helper_data1, helper_data2, helper_data3"))
  expect_silent(validate_depends(path = "./helper_example_repo", depends = "helper_data1,helper_data2,helper_data3"))
})

test_that("validate_depends() accepts single file", {
  expect_silent(validate_depends(path = "./helper_example_repo", depends = "helper_data1"))
})

test_that("validate_depends() throws error when one YAML file is missing", {
  # only missing files
  expect_error(validate_depends(path = "./helper_example_repo", depends = "non_existent_data"),
               "Missing YAML.*data/non_existent_data\\.yaml")
  # mix of missing and existing files
  expect_error(validate_depends(path = "./helper_example_repo", depends = "helper_data1, non_existent_data, helper_data3"),
               "Missing YAML.*data/non_existent_data\\.yaml")
})

test_that("validate_depends() throws error when multiple YAML files are missing", {
  # only missing files
  expect_error(validate_depends(path = "./helper_example_repo", depends = "non_existent_data1, non_existent_data2"),
               "Missing YAML.*data/non_existent_data1\\.yaml.*data/non_existent_data2\\.yaml")
  # mix of missing and existing files
  expect_error(validate_depends(path = "./helper_example_repo", depends = "non_existent_data1, helper_data1, non_existent_data2"),
               "Missing YAML.*data/non_existent_data1\\.yaml.*data/non_existent_data2\\.yaml")
})


test_that("validate_depends() throws error on empty string", {
  expect_error(validate_depends(depends = ""), "depends string must contain valid dataset names")
})

test_that("validate_depends() rejects extra commas", {
  expect_error(validate_depends(path = "./helper_example_repo", depends = "helper_data1,, helper_data2, helper_data3"), "depends string must contain valid dataset names")
})

test_that("validate_depends() rejects non-string input", {
  expect_error(validate_depends(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_depends(TRUE), "Must be of type 'string', not 'logical'.")
  expect_error(validate_depends(NA), "May not be NA.")
})


# Clean-up ---------------------------------------------------------------------

unlink(test_dir, recursive = TRUE)


