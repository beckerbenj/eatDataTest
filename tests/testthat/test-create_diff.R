
gads1 <- eatGADS::import_DF(data.frame(v1 = c(1, 1, 3, 3),
                                       v2 = c("a", "a", "b", "c"),
                                       id = 1:4))
gads2 <- eatGADS::import_DF(data.frame(v1 = c(1, 1, 2, 2),
                                       v2 = c("b", "a", "b", "d"),
                                       id = 1:4))


test_that("create pretty cross table", {
  out <- cross_table_as_data_frame(gads1$dat$v2, gads2$dat$v2,
                                   name1 = "gads1", name2 = "gads2", useNA = "ifany")

  expect_equal(as.character(out[1, 2]), "gads2")
  expect_equal(as.character(out[2, 1]), "gads1")
  expect_equal(as.character(out[2, 3:5]), sort(unique(gads2$dat$v2)))
  expect_equal(as.character(out[3:5, 2, drop = TRUE]), sort(unique(gads1$dat$v2)))
})


test_that("compare actual data", {
  out <- compare_actual_data(gads1, gads2, ID_var = "id")

  expect_equal(out$vars_with_differences,
               data.frame(varName = c("v1", "v2"), varLabel = NA_character_),
               ignore_attr = TRUE)
  expect_equal(as.character(out$v1[1, ]), c(NA, "oldrel", NA, NA))
  expect_equal(as.character(out$v1[2, ]), c("release", NA, 1, 2))
})


test_that("input validation create_diff", {
  expect_error(create_diff(.path = test_path("helper_example_repo"),
                               name = "helper_data_no_oldrel", ID_var = "ID"),
               "No oldrel path specified. No meaningful diff can be computed.")
})

test_that("create data and meta data diff", {
  suppressMessages(create_diff(.path = test_path("helper_example_repo"),
                                    name = "helper_data1", ID_var = "ID"))

  data_path <- test_path("helper_example_repo/diff/helper_data1_data_diff.xlsx")
  meta_path <- test_path("helper_example_repo/diff/helper_data1_meta_diff.xlsx")

  expect_equal(readxl::excel_sheets(meta_path),
               c("not_in_release_data", "not_in_oldrel_data",
                "differences_variable_labels", "differences_value_labels"))
  expect_equal(readxl::read_xlsx(meta_path, sheet = 2)$varName, c("info", "age", "career"))
  expect_equal(readxl::read_xlsx(meta_path, sheet = 3)$varName, c("ID_name", "school"))

  expect_equal(readxl::excel_sheets(data_path)[1:4], c("vars_with_differences",
                                                  "ID", "ID_name", "sex"))
  suppressMessages(sheet2 <- readxl::read_xlsx(data_path, sheet = 2, col_names = FALSE))
  expect_equal(as.character(sheet2[1, 1:2]),
               c(NA, "oldrel"))
  expect_equal(as.character(sheet2[2, 1:2]),
               c("release", NA))

})
