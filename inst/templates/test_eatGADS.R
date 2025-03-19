# Tests for compliance with eatGADS standards
# edit this file to modify and customize your tests

# import data set
dat <- eatDataTest::import_data(name = "{{ name }}", data_version = "release")

test_that("dat has proper GADSdat structure", {
  expect_s3_class(dat, "GADSdat")
})

test_that("there is a unique identifier variable", {
  out <- eatFDZ::check_id(dat, idVar = "IDSTUD")
  expect_equal(nrow(out$missing_ids), 0)
  expect_equal(nrow(out$duplicate_ids), 0)
})

test_that("variable names do not violate naming conventions", {
  out <- eatFDZ::check_var_names(dat)
  expect_equal(nrow(out), 0)
})

test_that("meta data only uses ASCII characters", {
  out <- eatFDZ::check_meta_encoding(dat)
  expect_equal(nrow(out), 0)
})

test_that("variable labels are provided for all variables", {
  out <- eatFDZ::check_var_labels(dat)
  expect_equal(nrow(out), 0)
})

test_that("Missing tags are defined for all values in range -50 to -99 with value labels", {
  out <- eatFDZ::check_missing_range(dat, missingRange = -50:-99)
  expect_equal(nrow(out), 0)
})

test_that("Missing tags are defined for all value labels with missing regular expression", {
  out <- eatFDZ::check_missing_regex(dat, missingRegex = "miss|Miss|not reached|omission")
  expect_equal(nrow(out), 0)
})

test_that("Value labels are provided for all variables", {
  out <- eatGADS::checkMissingValLabels(dat, vars = eatGADS::namesGADS(dat), output = "data.frame")
  expect_equal(nrow(out), 0)
})

