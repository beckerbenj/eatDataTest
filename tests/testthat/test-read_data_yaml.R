
#create_data_yaml(.path = "tests/testthat", name = "helper_data1", version = "v2.3",
#                 release_path = "C:/temp/data1.sav", oldrel_path = "C:/temp/data1_old.sav")


test_that("write data yaml", {
  out <- read_data_yaml(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out$version, "v2.3")
  expect_equal(out$release, "helper_example_repo/data_files/example_data.sav")
  expect_equal(out$oldrel, "helper_example_repo/data_files/example_data2.sav")
})

test_that("get release path", {
  out <- get_release_path(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out, "helper_example_repo/data_files/example_data.sav")
})

test_that("get oldrel path", {
  out <- get_oldrel_path(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out, "helper_example_repo/data_files/example_data2.sav")
})

test_that("get version number", {
  out <- get_version(.path = test_path("helper_example_repo"), name = "helper_data1")

  expect_equal(out, "v2.3")
})
