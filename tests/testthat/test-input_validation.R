
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

test_that("input is a valid string", {
  expect_error(validate_directory_path(123),
               "Assertion on 'path' failed: Must be of type 'string', not 'double'.")
  expect_error(validate_directory_path(TRUE),
               "Assertion on 'path' failed: Must be of type 'string', not 'logical'.")
  expect_error(validate_directory_path(c("./path1", "./path2")),
               "Assertion on 'path' failed: Must have length 1.")
})


test_that("fails for non-existing directories", {
  non_existent_path <- tempfile()
  expect_error(validate_directory_path(non_existent_path))
})


test_that("subdierectories are checked", {

  # validation test should pass because all directories exist
  expect_silent(validate_directory_path(test_dir))

  # validation test should fail if one subdirectory is missing

  unlink(changelog_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir))
  dir.create(changelog_dir)

  unlink(tests_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir))
  dir.create(tests_dir)

  unlink(data_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir))
  dir.create(data_dir)

  unlink(diff_dir, recursive = TRUE)
  expect_error(validate_directory_path(test_dir))
  dir.create(diff_dir)

})


