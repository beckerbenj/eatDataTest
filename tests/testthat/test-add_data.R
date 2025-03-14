

temp_dir <- tempdir()

changelog_dir <- file.path(temp_dir, "changelogs")
if(!file.exists(changelog_dir)) {
  dir.create(changelog_dir)
}
tests_dir <- file.path(temp_dir, "tests")
if(!file.exists(tests_dir)) {
  dir.create(tests_dir)
}



test_that("write changelog md", {
  create_changelog(.path = temp_dir, name = "data1")

  out <- readLines(file.path(temp_dir, "changelogs", "data1.md"))

  expect_equal(out[1], "# v1.0")
  expect_equal(out[2], "-initial release")
})

test_that("write tests R", {
  create_tests(.path = temp_dir, name = "data1")

  out <- readLines(file.path(temp_dir, "tests", "tests-data1.R"))

  expect_equal(out[1], 'dat <- import_data(name = \"data1\", data_version = \"release\")')
  expect_equal(out[2], '')
  expect_equal(out[3], 'test_that(\"test GADSdat structure\", {')
  expect_equal(out[4], '  expect_is(dat, \"GADSdat\")')
  expect_equal(out[5], '})')
})

test_that("write initial test results", {
  create_initial_test_result(.path = temp_dir, name = "data1")

  expect_true(file.exists(file.path(temp_dir, "tests", "result-data1.svg")))
})

# delete temporary directory
unlink(changelog_dir, recursive = TRUE)
