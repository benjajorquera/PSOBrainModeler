context("PSOHelpers Tests")

test_that("extract_and_round_pso_params works correctly", {
  params <- c(1.234, 0.5678, 0.00089, 2.345)
  result <-
    extract_and_round_pso_params(params, has_gamma = TRUE, n_lags = 1)
  
  expect_true(is.list(result))
  expect_equal(result$cost, 1.23)
  expect_equal(result$nu, 0.57)
  expect_equal(result$gamma, 0.00089)
  expect_equal(result$lags, 2)
})

test_that("pso_objective_selector selects the right function", {
  result_fir <- pso_objective_selector("FIR")
  #result_multi_fir <- pso_objective_selector("FIR", multi = TRUE)
  result_arx <- pso_objective_selector("ARX")
  #result_multi_arx <- pso_objective_selector("ARX", multi = TRUE)
  
  expect_identical(result_fir, pso_fir)
  #expect_identical(result_multi_fir, pso_multi_fir)
  expect_identical(result_arx, pso_arx)
  #expect_identical(result_multi_arx, pso_multi_arx)
})
