context("Tests for vsvr_model function")

# 1. Validate data type
test_that("Error when 'data' is not a data.frame", {
  expect_error(vsvr_model(matrix(1:4, 2), "y", 1, 0.5, 0.01, 0.001))
})

# 2. Validate response_var
test_that("Error when 'response_var' is not a single character string", {
  expect_error(vsvr_model(data, c("y", "x"), 1, 0.5, 0.01, 0.001))
  expect_error(vsvr_model(data, TRUE, 1, 0.5, 0.01, 0.001))
})

test_that("Error when response variable not found in data", {
  expect_error(vsvr_model(data, "z", 1, 0.5, 0.01, 0.001))
})
