
#browser()

curr_dir <- getwd()
#on.exit(setwd(curr_dir))
setwd("..")
setwd("..")

#dat_path <- get_release_path(.path = getwd(), name = "helper_data1")
dat_path <- get_release_path(.path = test_path("helper_example_repo"), name = "helper_data1")
dat <- eatGADS::import_spss(dat_path)


test_that("Data contains right number of rows", {
  expect_equal(nrow(dat$dat), 10)
})

test_that("Data contains important variables", {
  expect_true(all(c("ID", "sex", "age") %in% names(dat$dat)))
})
