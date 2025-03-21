
test_that("data import for NULL", {
  expect_error(import_data(.path = test_path("helper_example_repo"),
                          name = "helper_data_no_oldrel", data_version = "oldrel"),
               "No path specified for helper_data_no_oldrel in version oldrel. Please check data/helper_data_no_oldrel.yaml")
})

test_that("data import from .sav", {
  sav_gads <- import_data(.path = test_path("helper_example_repo"),
                          name = "helper_data1")

  expect_equal(nrow(sav_gads$dat), 10)
})

test_that("data import from .RDS", {
  RDS_gads <- import_data(.path = test_path("helper_example_repo"),
                          name = "helper_data2")

  expect_equal(nrow(RDS_gads$dat), 10)
})

test_that("data import from other formats", {
  expect_error(import_data(.path = test_path("helper_example_repo"), name = "helper_data3"),
               "Unsupported file extension: RData")
})
