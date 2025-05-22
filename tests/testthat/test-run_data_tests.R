

test_that("run tests via testthat", {
  output1 <- capture_output(suppressMessages(out <- run_tests_via_testthat(
    test_path("helper_tests/example_passing_test.R"))))
  expect_equal(basename(out), "pass_icon.svg")

  output2 <- capture_output(suppressMessages(out2 <- run_tests_via_testthat(
    test_path("helper_tests/example_failing_test.R"))))
  expect_equal(basename(out2), "fail_icon.svg")

  output3 <- capture_output(suppressMessages(out3 <- run_tests_via_testthat(
    test_path("helper_tests/example_outside_failing_test.R"))))
  expect_equal(basename(out3), "fail_icon.svg")
})

test_that("run successfull data tests", {
  out <- capture_output(
    mess <- capture_messages(
      run_data_tests(.path = test_path("helper_example_repo"),
                 name = "helper_data1")))

  expect_false(grepl("FAIL 1 ", out[2]))
  expect_false(grepl("FAIL 1 ", out[3]))
  expect_false(grepl("FAIL 1 ", out[4]))

  expect_true(grepl("Running tests for data set helper_data1:\n", mess[1]))
  expect_true(grepl("All tests passing!\n", mess[2]))
  expect_true(grepl("Updating icon...\n", mess[3]))
  expect_true(grepl("Updating version...\n", mess[4]))
})

test_that("run failing data tests", {
  out <- capture_output(
    mess <- capture_messages(
      run_data_tests(.path = test_path("helper_example_repo"),
                     name = "helper_data5")))

  expect_true(grepl("FAIL 1 ", out[1]))

  expect_true(grepl("Running tests for data set helper_data5:\n", mess[1]))
  expect_true(grepl("Some tests failing!\n", mess[2]))
  expect_true(grepl("Updating icon...\n", mess[3]))
})

test_that("run successfull data tests", {
  f <- tempfile(fileext = ".svg")
  create_and_download_badge("v1.2", download_path = f)

  out <- readLines(f, warn = FALSE)
  reference_badge <- readLines(test_path("helper_version_1_2_black.svg"), warn = FALSE)

  expect_equal(out, reference_badge)
})


# TODO: actually check for all .svgs
