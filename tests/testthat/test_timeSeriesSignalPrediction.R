context("Tests for generate_signal_response_predictions function")

test_that("generate_signal_response_predictions works correctly", {
  data_test <-
    data.frame(
      feature1 = rnorm(100),
      feature2 = rnorm(100),
      feature1_1 = rnorm(100),
      feature2_1 = rnorm(100),
      feature1_2 = rnorm(100),
      feature2_2 = rnorm(100)
    )
  
  pressure_data_test <- data.frame(feature1 = rnorm(50))
  
  test <- generate_signal_response_predictions(
    data_test,
    pressure_data_test,
    10,
    50,
    c("feature1", "feature2"),
    initial_columns_lags = c(2),
    2,
    initial_column_names = c("feature1"),
    c(1),
    "feature2",
    0.5,
    1,
    0.5,
    NULL,
    1
  )
  
  # Test: is the result a dataframe?
  expect_is(test, "data.frame")
  
  # Test: does it have only 1 column?
  expect_equal(ncol(test), 1)
  
  # Test: is the column name "feature2"?
  expect_equal(names(test), "feature2")
  
  # Test: does it have 50 observations?
  expect_equal(nrow(test), 50)
  
  # Test: are the first 10 observations in "feature2" equal to 1?
  expect_equal(test$feature2[1:10], rep(0.5, 10))
  
  # Placeholder variables for specific test cases
  placeholder_data_test <- "not_a_dataframe"
  placeholder_df <- "not_a_dataframe"
  placeholder_int <- -1
  empty_list <- list()
  
  # Test: is data a valid data frame?
  test_that("Data is a valid data frame", {
    expect_error(
      generate_signal_response_predictions(
        placeholder_data_test,
        pressure_data_test,
        10,
        50,
        c("feature1", "feature2"),
        initial_columns_lags = c(2),
        2,
        initial_column_names = c("feature1"),
        c(1),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "data must be a dataframe"
    )
  })
  
  # Test: is pressure_signal_df a dataframe?
  test_that("pressure_signal_df is a dataframe", {
    expect_error(
      generate_signal_response_predictions(
        data_test,
        placeholder_df,
        10,
        50,
        c("feature1", "feature2"),
        initial_columns_lags = c(2),
        2,
        initial_column_names = c("feature1"),
        c(1),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "pressure_signal_df must be a dataframe"
    )
  })
  
  # Test: are pressure_start and prediction_size positive integers?
  test_that("pressure_start and prediction_size are positive integers", {
    expect_error(
      generate_signal_response_predictions(
        data_test,
        pressure_data_test,
        placeholder_int,
        50,
        c("feature1", "feature2"),
        initial_columns_lags = c(2),
        2,
        initial_column_names = c("feature1"),
        c(1),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "pressure_start and prediction_size must be positive integers"
    )
  })
  
  # Test: Validate input vectors and column sizes
  test_that("Input vectors and column sizes are valid", {
    expect_error(
      generate_signal_response_predictions(
        data_test,
        pressure_data_test,
        10,
        50,
        empty_list,
        initial_columns_lags = c(2),
        2,
        initial_column_names = c("feature1"),
        c(1),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "Error: Empty vectors or mismatched sizes between column names and values"
    )
    
    expect_error(
      generate_signal_response_predictions(
        data_test,
        pressure_data_test,
        10,
        50,
        c("feature1", "feature2"),
        initial_columns_lags = c(2),
        2,
        empty_list,
        c(1),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "Error: Empty vectors or mismatched sizes between column names and values"
    )
    
    expect_error(
      generate_signal_response_predictions(
        data_test,
        pressure_data_test,
        10,
        50,
        c("feature1", "feature2"),
        initial_columns_lags = c(2),
        2,
        c("feature1"),
        c(1, 2),
        "feature2",
        0.5,
        1,
        0.5,
        NULL,
        1
      ),
      "Error: Empty vectors or mismatched sizes between column names and values"
    )
  })
})