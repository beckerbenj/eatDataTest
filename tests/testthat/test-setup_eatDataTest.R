
test_dir <- file.path(tempdir(), "test_repo")
dir.create(test_dir)

test_that("create directoy with cli messaging", {
  expect_message(create_directory(test_dir, name = "test1"),
                 "Created test1 folder.")
  expect_message(create_directory(test_dir, name = "test1"),
                 "test1 folder already exists.")
})

test_that("load readme setup", {
  out <- load_readme_setup()

  expect_equal(out[1], "## Status Data Sets")
  expect_equal(out[3], "| GADS                    | Version | Tests              |")
  expect_equal(out[8],  "[0. Call `eatDataTest::add_data()` to setup architecture for a new data set.]")
  expect_equal(out[9], "1. Archive old data set version")
})

if(clipr::clipr_available()) {
  test_that("full setup eatDataTest", {
    suppressMessages(out <- setup_eatDataTest(.path = test_dir))

    expect_true(dir.exists(file.path(test_dir, "data")))
    expect_true(dir.exists(file.path(test_dir, "changelogs")))
    expect_true(dir.exists(file.path(test_dir, "tests")))
    expect_true(dir.exists(file.path(test_dir, "diff")))

    expect_equal(out[1], "## Status Data Sets")
    expect_equal(out[3], "| GADS                    | Version | Tests              |")
    expect_equal(out[8],  "[0. Call `eatDataTest::add_data()` to setup architecture for a new data set.]")
    expect_equal(out[9], "1. Archive old data set version")
  })
}

# delete temporary directory
unlink(test_dir, recursive = TRUE)
