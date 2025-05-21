

test_that("write data yaml", {
  out <- read_data_yaml(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out$version, "v2.3")
  expect_equal(out$release,
               tools::file_path_as_absolute(test_path("helper_example_repo/data_files/example_data.sav")))
  expect_equal(out$oldrel,
               tools::file_path_as_absolute(test_path("helper_example_repo/data_files/example_data2.sav")))
})

test_that("get release path", {
  out <- get_release_path(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out,
               tools::file_path_as_absolute(test_path("helper_example_repo/data_files/example_data.sav")))
})

test_that("get oldrel path", {
  out <- get_oldrel_path(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out,
               tools::file_path_as_absolute(test_path("helper_example_repo/data_files/example_data2.sav")))
})

test_that("get version number", {
  out <- get_version(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out, "v2.3")
})


#test_that("validate name", {
#  out_exists <- validate_name(.path = test_path("helper_example_repo"), name = "helper_data1")
#  out_not_exists <- validate_name(.path = test_path("helper_example_repo"), name = "helper_data99")
#
#  expect_equal(out_exists, TRUE)
#  expect_equal(out_not_exists, FALSE)
#})

#test_that("validate name", {
#  out_exists <- validate_depends(.path = test_path("helper_example_repo"),
#                              depends = c("helper_data1", "helper_data3"))
#  out_not_exists <- validate_depends(.path = test_path("helper_example_repo"),
#                                 depends = c("helper_data1", "helper_data99"))
#
#  expect_equal(out_exists, TRUE)
#  expect_equal(out_not_exists, FALSE)
#})

