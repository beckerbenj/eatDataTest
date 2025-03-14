
#setwd("C:/Repositories/Packages/eatDataTest/tests/testthat/helper_example_repo")
#setwd("C:/Repositories/Packages/eatDataTest")
dat <- import_data(name = "helper_data1", data_version = "release")

test_that("Data contains right number of rows", {
  expect_equal(nrow(dat$dat), 10)
})

test_that("Data contains important variables", {
  expect_true(all(c("ID", "sex", "age") %in% names(dat$dat)))
})
