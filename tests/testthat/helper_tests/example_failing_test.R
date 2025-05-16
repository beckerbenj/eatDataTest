

test_that("a passing test", {
  expect_equal(1+1, 2)
})

test_that("a failing test", {
  expect_equal(1+1, 3)
})
