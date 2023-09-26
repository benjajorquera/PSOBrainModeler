context("Tests for pso_model function")

test_that("Test pso_model function", {
  # Mocking required functions
  extract_and_round_pso_params <-
    function(...)
      return(list(
        cost = 1,
        nu = 2,
        gamma = 3,
        lags = c(1, 2)
      ))
  pso_training_model <- function(...)
    return("Training Completed")
  .psoBrainModelerEnv <- new.env()
  assign("NORM_VSVR_RESPONSE", "Test_Response", envir = .psoBrainModelerEnv)
  
  # Testing incorrect params input
  testthat::expect_error(pso_model(params = c(1, 2), model = "ARX"))
  
  # Testing incorrect model input
  testthat::expect_error(pso_model(params = c(1, 2, 3, 4), model = "INVALID"))
})
