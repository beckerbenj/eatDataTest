

test_that("get all data names", {
  out <- get_all_data_names(.path = test_path("helper_example_repo"))

  expect_contains(out, c("helper_data_no_oldrel", "helper_data1", "helper_data2", "helper_data3"))
})


test_that("run reverse dependency tests", {
  expect_message(run_revdep_tests(.path = test_path("helper_example_repo"), name = "helper_data1"),
                 "No reverse dependencies found.")

  expect_message(out <- capture_output(run_revdep_tests(.path = test_path("helper_example_repo"),
                                                 name = "helper_data2")),
                 "Running tests for helper_data1")

})
