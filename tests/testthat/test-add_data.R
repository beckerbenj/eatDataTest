

test_dir <- file.path(tempdir(), "test_repo")
dir.create(test_dir)

changelog_dir <- file.path(test_dir, "changelogs")
dir.create(changelog_dir)

tests_dir <- file.path(test_dir, "tests")
dir.create(tests_dir)

data_dir <- file.path(test_dir, "data")
dir.create(data_dir)

test_that("write changelog md", {
  create_changelog(.path = test_dir, name = "data1")

  out <- readLines(file.path(test_dir, "changelogs", "data1.md"))

  expect_equal(out[1], "# v1.0")
  expect_equal(out[2], "-initial release")
})

test_that("write tests R", {
  create_tests(.path = test_dir, name = "data1")

  out <- readLines(file.path(test_dir, "tests", "test-data1.R"))

  expect_equal(out[5], 'dat <- eatDataTest::import_data(name = \"data1\", data_version = \"release\")')
  expect_equal(out[6], '')
  expect_equal(out[7], 'test_that(\"dat has proper GADSdat structure\", {')
})

test_that("write initial test results", {
  create_initial_test_result(.path = test_dir, name = "data1")

  expect_true(file.exists(file.path(test_dir, "tests", "result-data1.svg")))
})

test_that("create readme snippet", {
  out <- create_readme_snippet(name = "data1", version = "v1.0")

  expect_equal(out,
               "| data1              | ![s](tests/version-data1.svg)    | ![s](tests/result-data1.svg) |")
})

if(clipr::clipr_available()) {
  test_that("full add data workflow", {
    suppressMessages(out <- add_data(.path = test_dir, name = "data1", release_path = "C:/temp/data1.sav",
                                     version = "v1.0"))

    expect_equal(out,
                 "| data1              | ![s](tests/version-data1.svg)    | ![s](tests/result-data1.svg) |")
  })
}

# delete temporary directory
unlink(test_dir, recursive = TRUE)
