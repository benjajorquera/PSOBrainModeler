context("Tests for Helpers")

test_that("validate_inputs_main works correctly", {
  data <- data.frame(signal = rnorm(100), predictor = rnorm(100))
  
  # Positive test
  expect_silent(
    validate_inputs_main(
      data = data,
      model = "FIR",
      signal_names = c("signal"),
      predictors_names = c("predictor"),
      vsvr_response = "signal"
    )
  )
  
  # Negative tests
  expect_error(
    validate_inputs_main(
      data = NULL,
      model = "FIR",
      signal_names = c("signal"),
      predictors_names = c("predictor"),
      vsvr_response = "signal"
    )
  )
  
  expect_error(
    validate_inputs_main(
      data = data,
      model = "INVALID_MODEL",
      signal_names = c("signal"),
      predictors_names = c("predictor"),
      vsvr_response = "signal"
    )
  )
  
})