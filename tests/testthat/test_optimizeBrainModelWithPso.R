context("Tests for Optimize Brain Model With PSO function")

default_params <- list(
  config = list(),
  psoptim_config = list(),
  data = data.frame(),
  model = "MODEL",
  multi = FALSE,
  signal_names = c("Feature1", "Feature2"),
  excluded_cols = c("Feature3", "Feature4"),
  predictors_names = c("Feature1"),
  params_lower_bounds = c(1, 0.1, 1),
  params_upper_bounds = c(10, 0.5, 3),
  params_initial_values = c(NA, NA, NA),
  vsvr_response = "Feature2"
)

brain_modeler_config <- configure_pso_brain_modeler()
psoptim_config <- configure_psoptim_control()

# Test for PSOBrainModelerConfig validation
test_that("test PSOBrainModelerConfig validation", {
  test_params <- default_params
  expect_error(
    do.call(optimize_brain_model_with_PSO, test_params),
    "Please provide a valid configuration."
  )
})

# Test for PSOBrainModelerPSOPTIMConfig validation
test_that("test PSOBrainModelerPSOPTIMConfig validation", {
  test_params <- default_params
  test_params$config <- brain_modeler_config
  expect_error(
    do.call(optimize_brain_model_with_PSO, test_params),
    "Please provide a valid 'psoptim' configuration."
  )
})

# Test for model parameter validation
test_that("test model parameter validation", {
  test_params <- default_params
  test_params$config <- brain_modeler_config
  test_params$psoptim_config <- psoptim_config
  expect_error(
    do.call(optimize_brain_model_with_PSO, test_params),
    "Invalid model specified. Please select from 'FIR', 'NFIR', 'ARX', or 'NARX'."
  )
})

# Test for parameter vectors validation
test_that("test parameter vectors validation", {
  test_params <- default_params
  test_params$config <- brain_modeler_config
  test_params$psoptim_config <- psoptim_config
  test_params$model <- "FIR"
  test_params$params_initial_values <- c(1, 0.1)
  expect_error(
    do.call(optimize_brain_model_with_PSO, test_params),
    "Length of 'params_lower_bounds', 'params_upper_bounds', and 'params_initial_values' must be equal."
  )
})
