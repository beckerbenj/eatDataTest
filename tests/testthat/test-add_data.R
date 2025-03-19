

temp_dir <- tempdir()

changelog_dir <- file.path(temp_dir, "changelogs")
if(!file.exists(changelog_dir)) {
  dir.create(changelog_dir)
}
tests_dir <- file.path(temp_dir, "tests")
if(!file.exists(tests_dir)) {
  dir.create(tests_dir)
}
data_dir <- file.path(temp_dir, "data")
if(!file.exists(data_dir)) {
  dir.create(data_dir)
}

test_that("copy empty GADSdat into data", {
  out <- create_empty_oldrel(.path = temp_dir, name = "data1")

  expect_equal(out, file.path(temp_dir, "data", "empty_gads.RDS"))
  expect_true(file.exists(out))
})

test_that("write changelog md", {
  create_changelog(.path = temp_dir, name = "data1")

  out <- readLines(file.path(temp_dir, "changelogs", "data1.md"))

  expect_equal(out[1], "# v1.0")
  expect_equal(out[2], "-initial release")
})

test_that("write tests R", {
  create_tests(.path = temp_dir, name = "data1")

  out <- readLines(file.path(temp_dir, "tests", "test-data1.R"))

  expect_equal(out[5], 'dat <- eatDataTest::import_data(name = \"data1\", data_version = \"release\")')
  expect_equal(out[6], '')
  expect_equal(out[7], 'test_that(\"dat has proper GADSdat structure\", {')
})

test_that("write initial test results", {
  create_initial_test_result(.path = temp_dir, name = "data1")

  expect_true(file.exists(file.path(temp_dir, "tests", "result-data1.svg")))
})

test_that("create readme snippet", {
  out <- create_readme_snippet(name = "data1", version = "v1.0")

  expect_equal(out,
               "| data1              | v1.0    | ![s](tests/result-data1.svg) |")
})

if(clipr::clipr_available()) {
  test_that("full add data workflow", {
    out <- add_data(.path = temp_dir, name = "data1", release_path = "C:/temp/data1.sav", version = "v1.0")

    expect_equal(out,
                 "| data1              | v1.0    | ![s](tests/result-data1.svg) |")
  })
}

# delete temporary directory
unlink(changelog_dir, recursive = TRUE)
