context("Test Score Signal")

test_that("Test basic filter for evaluate_signal_quality", {
  # Test setup
  sample_signal <-
    rnorm(50)
  start_point <- 5
  
  # Basic filter tests
  expect_equal(evaluate_signal_quality(sample_signal, start_point),-10)
  
  # Signal too short
  expect_error(evaluate_signal_quality(sample_signal[1:25], start_point))
  
  # Signal not numeric
  expect_error(evaluate_signal_quality(as.character(sample_signal), start_point))
})
