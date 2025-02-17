

temp_dir <- tempdir()
data_dir <- file.path(temp_dir, "data")
if(!file.exists(data_dir)) {
  dir.create(data_dir)
}

test_that("write data yaml", {
  create_data_yaml(.path = temp_dir, name = "data1", release_path = "C:/temp/data1.sav")

  out <- yaml::yaml.load_file(file.path(temp_dir, "data", "data1.yaml"))

  expect_equal(out$version, "v1.0")
  expect_equal(out$release, "C:/temp/data1.sav")
  expect_equal(out$oldrel, "")
})

# delete temporary directory
unlink(data_dir, recursive = TRUE)
