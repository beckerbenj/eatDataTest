
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

# save a dataframe
saveRDS(head(iris), file.path(test_dir, "iris_release.Rds"))

# save a random text file
writeLines("This is not a data frame.", file.path(test_dir, "iris_release.txt"))



# validate_directory_path ------------------------------------------------------

test_that("validate_directory_path() checks valid strings", {
  expect_error(validate_directory_path(123), "Must be of type 'string', not 'double'.")
  expect_error(validate_directory_path(TRUE), "Must be of type 'string', not 'logical'.")
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
  expect_error(validate_directory_path(test_dir), "Missing required subdirectory: 'changelogs/'")
  dir.create(changelog_dir)

  unlink(tests_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "Missing required subdirectory: 'tests/'")
  dir.create(tests_dir)

  unlink(data_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "Missing required subdirectory: 'data/'")
  dir.create(data_dir)

  unlink(diff_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir), "Missing required subdirectory: 'diff/'")
  dir.create(diff_dir)

})


# validate_version -------------------------------------------------------------

test_that("validate_version() accepts correct version format", {
  expect_silent(validate_version("v1.0"))
  expect_silent(validate_version("v123.456"))
})

test_that("validate_version() fails if input is not a string", {
  expect_error(validate_version(1.0), "Must be of type 'string', not 'double'.")
  expect_error(validate_version(TRUE), "Must be of type 'string', not 'logical'.")
})

test_that("validate_version() requires version to start with 'v'", {
  expect_error(validate_version("1.0"), "must start with a lowercase 'v'")
})

test_that("validate_version() requires digits after 'v'", {
  expect_error(validate_version("v"), "have one or more digits immediately after")
  expect_error(validate_version("v."), "have one or more digits immediately after")
})

test_that("validate_version() requires a dot after digits", {
  expect_error(validate_version("v1"), "dot after the digits")
})

test_that("validate_version() requires digits after the dot", {
  expect_error(validate_version("v1."), "end with one or more digits after the dot")
  expect_error(validate_version("v1.a"), "end with one or more digits after the dot")
})

test_that("validate_version() rejects too complex versions", {
  expect_error(validate_version("v1.0.2"), "end with one or more digits after the dot")
  expect_error(validate_version("v1.0beta"), "end with one or more digits after the dot")
})







