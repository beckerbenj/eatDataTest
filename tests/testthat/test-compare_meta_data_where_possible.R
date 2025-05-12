
gads1 <- eatGADS::import_DF(data.frame(v1 = c(1, 1, 3, 3),
                                       v2 = c("a", "a", "b", "c"),
                                       id = 1:4))
gads2 <- eatGADS::changeVarLabels(gads1, varName = "v1", varLabel = "A label")
gads2 <- eatGADS::changeVarLabels(gads1, varName = "v1", varLabel = "A label")

suppressMessages(gads1b <- eatGADS::removeVars(gads1, vars = "v2"))

gads2b <- eatGADS::changeVarNames(gads2, oldNames = "v1", newNames = "v1_imp")

test_that("input validation", {
  expect_error(compare_meta_data_where_possible(data1 = gads1, data2 = gads2, suffix = NULL, ID_var = "id2"),
               "The following 'ID_var' are not variables in the data1: id2")
})

test_that("compare meta data where possible", {
  out <- compare_meta_data_where_possible(data1 = gads1, data2 = gads2, suffix = NULL, ID_var = "id")

  expect_equal(length(out), 3)
  expect_equal(nrow(out$differences_value_labels), 0)
  expect_equal(nrow(out$differences_variable_labels), 1)
  expect_equal(out$compared_variables, c("v1", "v2", "id"))

  out2 <- compare_meta_data_where_possible(data1 = gads1b, data2 = gads2, suffix = NULL, ID_var = "id")

  expect_equal(nrow(out2$differences_value_labels), 0)
  expect_equal(nrow(out2$differences_variable_labels), 1)
  expect_equal(out2$compared_variables, c("v1", "id"))

  out3 <- compare_meta_data_where_possible(data1 = gads1, data2 = gads2b, suffix = "_imp", ID_var = "id")

  expect_equal(nrow(out3$differences_value_labels), 0)
  expect_equal(nrow(out3$differences_variable_labels), 1)
  expect_equal(out3$compared_variables, c("v1", "v2", "id"))

  out4 <- compare_meta_data_where_possible(data1 = gads1b, data2 = gads2b, suffix = "_imp", ID_var = "id")

})


