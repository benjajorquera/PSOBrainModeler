context("Test Score Signal")

test_that("Test basic filter for evaluate_signal_quality", {
  # Test setup
  sample_signal <-
    rnorm(50)
  start_point <- 5
  
  # Signal not numeric
  expect_error(evaluate_signal_quality(as.character(sample_signal), start_point))
})
