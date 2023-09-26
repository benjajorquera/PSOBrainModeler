context("Tests for generate_signal_response_predictions function")

# Create initial test data
data_test <- data.frame(
  feature1 = rnorm(100),
  feature2 = rnorm(100),
  feature1_1 = rnorm(100),
  feature2_1 = rnorm(100),
  feature1_2 = rnorm(100),
  feature2_2 = rnorm(100)
)

pressure_data_test <- data.frame(feature1 = rnorm(50))

# Define common parameters
common_params <- list(
  data = data_test,
  pressure_signal_df = pressure_data_test,
  pressure_start = 10,
  prediction_size = 50,
  column_names = c("feature1", "feature2"),
  initial_columns_lags = c(2),
  predicted_column_lags = NULL,
  initial_column_names = c("feature1"),
  initial_column_values = c(1),
  prediction_col_name = "feature2",
  prediction_initial_value = 0.5,
  predictor_cols = c("feature1"),
  cost = 1,
  nu = 0.5,
  gamma = NULL,
  tolerance = 1
)

# Test: generate_signal_response_predictions works correctly
test_that("generate_signal_response_predictions works correctly", {
  test <- do.call(generate_signal_response_predictions, common_params)
  
  # Assertions for expected output
  expect_is(test, "data.frame")
  expect_equal(ncol(test), 1)
  expect_equal(names(test), "feature2")
  expect_equal(nrow(test), 50)
  expect_equal(test$feature2[1:10], rep(0.5, 10))
})

# Placeholder variables for specific test cases
placeholder_data_test <- "not_a_dataframe"
placeholder_df <- "not_a_dataframe"
placeholder_int <- -1
empty_list <- list()

# Test: Ensure that 'data' is a valid dataframe
test_that("Data is a valid data frame", {
  altered_params <- common_params
  altered_params$data <- placeholder_data_test
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "data must be a dataframe"
  )
})

# Test: Ensure 'pressure_signal_df' is a dataframe
test_that("pressure_signal_df is a dataframe", {
  altered_params <- common_params
  altered_params$pressure_signal_df <- placeholder_df
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "pressure_signal_df must be a dataframe"
  )
})

# Test: Ensure 'pressure_start' and 'prediction_size' are positive integers
test_that("pressure_start and prediction_size are positive integers", {
  altered_params <- common_params
  altered_params$pressure_start <- placeholder_int
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "pressure_start and prediction_size must be positive integers"
  )
})

# Test: Validate input vectors and column sizes
test_that("Input vectors and column sizes are valid", {
  # Check with empty 'features' vector
  altered_params <- common_params
  altered_params$column_names <- empty_list
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "Error: Empty vectors or mismatched sizes between column names and values"
  )
  
  # Check with empty 'initial_column_names' vector
  altered_params$column_names <- c("feature1", "feature2")
  altered_params$initial_column_names <- empty_list
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "Error: Empty vectors or mismatched sizes between column names and values"
  )
  
  # Check with mismatched sizes between 'initial_column_names' and 'initial_column_values'
  altered_params$initial_column_names <- c("feature1")
  altered_params$initial_column_values <- c(1, 2)
  
  expect_error(
    do.call(generate_signal_response_predictions, altered_params),
    "Error: Empty vectors or mismatched sizes between column names and values"
  )
})
