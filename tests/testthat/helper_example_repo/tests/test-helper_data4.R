## currently not used

dat <- import_data(name = "helper_data4", data_version = "release")

test_that("Data contains right number of rows", {
  expect_equal(nrow(dat$dat), 8)
})

test_that("Data contains important variables", {
  expect_true(all(c("ID", "sex", "age") %in% names(dat$dat)))
})
