context("PSOHelpers Tests")

test_that("extract_and_round_pso_params works correctly", {
  params <- c(
    cost = 1.234,
    nu = 0.5678,
    gamma = 0.00089,
    lags = 2
  )
  result <-
    extract_and_round_pso_params(params, has_gamma = TRUE, n_lags = 1)
  
  expect_true(is.list(result))
  # expect_equal(result$cost, 1.23)
  # expect_equal(result$nu, 0.57)
  # expect_equal(result$gamma, 0.00089)
  # expect_equal(result$lags, 2)
})