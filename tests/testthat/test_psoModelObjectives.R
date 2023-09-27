context("Tests for PSOModelObjectives")

# Test 1: Check 'params' length validation for FIR model
test_that("pso_model detects invalid 'params' length for FIR model", {
  expect_error(
    pso_model(
      params = c(1, 2),
      model = "FIR",
      data_list = list(NORM_VSVR_RESPONSE = "response")
    ),
    "params should be a numeric vector of length 3 or 4 for model FIR."
  )
})

# Test 2: Check 'params' length validation for ARX model
test_that("pso_model detects invalid 'params' length for ARX model", {
  expect_error(
    pso_model(
      params = c(1, 2, 3),
      model = "ARX",
      data_list = list(NORM_VSVR_RESPONSE = "response")
    ),
    "params should be a numeric vector of length 4 or 5 for model ARX."
  )
})