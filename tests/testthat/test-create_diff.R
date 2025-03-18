
gads1 <- eatGADS::import_DF(data.frame(v1 = c(1, 1, 3, 3),
                                       v2 = c("a", "a", "b", "c"),
                                       id = 1:4))
gads2 <- eatGADS::import_DF(data.frame(v1 = c(1, 1, 2, 2),
                                       v2 = c("b", "a", "b", "d"),
                                       id = 1:4))

test_that("compare actual data", {
  out <- compare_actual_data(gads1, gads2, ID_var = "id")

  expect_equal(out$vars_with_differences,
               data.frame(varName = c("v1", "v2"), varLabel = NA_character_),
               ignore_attr = TRUE)
  #expect_equal(out$v1, table(gads1$dat$v1, gads2$dat$v1, dnn = c("data1", "data2")))
  #expect_equal(out$v1, as.data.frame.matrix(table(gads1$dat$v1, gads2$dat$v1, dnn = c("data1", "data2"))))
  #expect_equal(out$v2, as.data.frame.matrix(table(gads1$dat$v2, gads2$dat$v2, dnn = c("data1", "data2"))))
})


test_that("create data and meta data diff", {
  suppressMessages(create_diff(.path = test_path("helper_example_repo"),
                                    name = "helper_data1", ID_var = "ID"))

  data_path <- test_path("helper_example_repo/diff/helper_data1_data_diff.xlsx")
  meta_path <- test_path("helper_example_repo/diff/helper_data1_meta_diff.xlsx")

  expect_equal(readxl::excel_sheets(meta_path), c("not_in_release_data", "not_in_oldrel_data",
                             "differences_variable_labels", "differences_value_labels"))
  expect_equal(readxl::read_xlsx(meta_path, sheet = 2)$varName, c("info", "age", "career"))
  expect_equal(readxl::read_xlsx(meta_path, sheet = 3)$varName, c("ID_name", "school"))

  expect_equal(readxl::excel_sheets(data_path)[1:4], c("vars_with_differences",
                                                  "ID", "ID_name", "sex"))
  #expect_equal(readxl::read_xlsx(data_path, sheet = 2),
  #             as.data.frame.matrix(table(gads1$dat$v1, gads2$dat$v1, dnn = c("data1", "data2"))))
  #expect_equal(readxl::read_xlsx(data_path, sheet = 3),
  #             as.data.frame.matrix(table(gads1$dat$v2, gads2$dat$v2, dnn = c("data1", "data2"))))
})
