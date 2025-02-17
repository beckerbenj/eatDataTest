

test_that("create data diff", {
  suppressMessages(create_data_diff(.path = test_path("helper_example_repo"),
                                    name = "helper_data1", ID_var = "ID"))

  out_path <- test_path("helper_example_repo/diff/helper_data1_diff.xlsx")

  expect_equal(readxl::excel_sheets(out_path), c("not_in_release_data", "not_in_oldrel_data",
                             "differences_variable_labels", "differences_value_labels"))

  expect_equal(readxl::read_xlsx(out_path, sheet = 2)$varName, c("info", "age", "career"))
  expect_equal(readxl::read_xlsx(out_path, sheet = 3)$varName, c("ID_name", "school"))
})
