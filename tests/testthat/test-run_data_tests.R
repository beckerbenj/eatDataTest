

test_that("run data tests", {
  out <- capture_output_lines(run_data_tests(.path = test_path("helper_example_repo"),
                                             name = "helper_data1"))

  expect_false(grepl("FAIL 1 ", out[2]))
  expect_false(grepl("FAIL 1 ", out[3]))
  expect_false(grepl("FAIL 1 ", out[4]))
})
