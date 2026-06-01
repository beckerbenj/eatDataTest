
temp_dir <- tempdir()
data_dir <- file.path(temp_dir, "data")
changelogs_dir <- file.path(temp_dir, "changelogs")
if(!file.exists(data_dir)) {
  dir.create(data_dir)
}
if(!file.exists(changelogs_dir)) {
  dir.create(changelogs_dir)
}

# Create temporary test files for validation tests
test_data_file_sav <- file.path(temp_dir, "test_data1.sav")
test_data_file_rds <- file.path(temp_dir, "test_data1.rds")
file.create(test_data_file_sav)
file.create(test_data_file_rds)

test_that("write data yaml with default values", {
  create_data_yaml(.path = temp_dir, name = "data1", release_path = test_data_file_sav)

  out <- yaml::yaml.load_file(file.path(temp_dir, "data", "data1.yaml"))

  expect_equal(out$version, "v1.0")
  expect_equal(out$release, test_data_file_sav)
  expect_equal(out$oldrel, NULL)
  expect_equal(out$depends, NULL)
})

test_that("write data yaml with all parameters", {
  # Create oldrel test file
  test_data_file_old_rds <- file.path(temp_dir, "test_data2_old.rds")
  file.create(test_data_file_old_rds)
  
  create_data_yaml(.path = temp_dir, name = "data2", 
                   release_path = test_data_file_rds,
                   oldrel_path = test_data_file_old_rds,
                   version = "v2.5",
                   depends = "data1")

  out <- yaml::yaml.load_file(file.path(temp_dir, "data", "data2.yaml"))

  expect_equal(out$version, "v2.5")
  expect_equal(out$release, test_data_file_rds)
  expect_equal(out$oldrel, test_data_file_old_rds)
  expect_equal(out$depends, "data1")
})

# Test input validation
test_that("validate_directory_path: fails for non-existent directory", {
  expect_error(create_data_yaml(.path = "/nonexistent/path", name = "data3", 
                                release_path = test_data_file_sav),
               "does not exist")
})

test_that("validate_directory_path: fails when required subdirectories missing", {
  incomplete_dir <- file.path(temp_dir, "incomplete")
  dir.create(incomplete_dir, showWarnings = FALSE)
  
  expect_error(create_data_yaml(.path = incomplete_dir, name = "data4", 
                                release_path = test_data_file_sav),
               "required subdirectories are missing")
})

test_that("validate_version: fails for invalid version format", {
  expect_error(create_data_yaml(.path = temp_dir, name = "data5", 
                                release_path = test_data_file_sav,
                                version = "1.0"),
               "must start with a lowercase 'v'")
  
  expect_error(create_data_yaml(.path = temp_dir, name = "data6", 
                                release_path = test_data_file_sav,
                                version = "v1"),
               "must include a dot")
})

test_that("validate_data_name: fails for invalid characters", {
  expect_error(create_data_yaml(.path = temp_dir, name = "data-with-dashes", 
                                release_path = test_data_file_sav),
               "must only contain letters, digits, or underscores")
  
  expect_error(create_data_yaml(.path = temp_dir, name = "data with spaces", 
                                release_path = test_data_file_sav),
               "must only contain letters, digits, or underscores")
})

test_that("validate_data_path: fails for non-existent file", {
  expect_error(create_data_yaml(.path = temp_dir, name = "data7", 
                                release_path = "/nonexistent/file.sav"),
               "does not exist")
})

test_that("validate_data_path: fails for invalid file extension", {
  expect_error(create_data_yaml(.path = temp_dir, name = "data8", 
                                release_path = file.path(temp_dir, "data.csv")),
               "Unsupported file extension|must have one of the following file extensions")
})

# Clean up temporary files and directories
unlink(test_data_file_sav)
unlink(test_data_file_rds)
unlink(test_data_file_old_rds, force = TRUE)
unlink(data_dir, recursive = TRUE)
unlink(changelogs_dir, recursive = TRUE)
