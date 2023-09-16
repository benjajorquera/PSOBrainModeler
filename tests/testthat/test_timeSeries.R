# Load Required Libraries
library(testthat)
library(dplyr)
library(signal)  # For butter() and filter() functions

# Source Utility Functions
source("TimeSeries.R")

# Define Global Variables
base_df <-
  data.frame(A = c(1, 2, 3),
             B = c(4, 5, 6),
             C = c(7, 8, 9))
norm_colnames <- c("A_norm", "B_norm", "C_norm")
empty_df <- data.frame()

# ------------------------------------------------------------
# Tests for Special Cases and Other Functions
# ------------------------------------------------------------

# Test add_pressure_step
test_that(
  "add_pressure_step generates correct data only for order 2 butterworth filter with 0.2 fs",
  {
    result <-
      add_pressure_step(
        pressure_start = 2,
        signal_end = 10,
        butter_order = 2,
        butter_fs = 0.2
      )
    expect_is(result, "data.frame")
    expect_equal(nrow(result), 10)
    expect_equal(length(unique(result$signal)), 9)
    expect_error(
      add_pressure_step(
        pressure_start = -1,
        signal_end = 10,
        butter_order = 1,
        butter_fs = 1
      )
    )
    expect_error(add_pressure_step(
      pressure_start = 2,
      signal_end = 2,
      butter_order = 1,
      butter_fs = 1
    ))
  }
)

# Test generate_time_series_data
test_that("generate_time_series_data functions correctly", {
  input_df <-
    data.frame(
      x = 1:10,
      y = 11:20,
      z = 21:30,
      x_1 = 2:11,
      y_1 = 12:21
    )
  result <-
    generate_time_series_data(
      input_df,
      data_cols = c("x", "y"),
      predictor_cols = NULL,
      lagged_cols = c("x", "y"),
      lag_values = c(1, 1),
      is_training = TRUE
    )
  expect_is(result, "data.frame")
  expect_equal(ncol(result), 4)
  expect_error(
    generate_time_series_data(
      NULL,
      data_cols = c("x", "y"),
      predictor_cols = NULL,
      lagged_cols = c("x", "y"),
      lag_values = c(1, 1),
      is_training = TRUE
    )
  )
})

# Test process_dataframe
test_that("process_dataframe works correctly", {
  df <- data.frame(x = 1:5, y = 6:10, z = 11:15)
  result <-
    process_dataframe(
      df,
      excluded_cols = c("z"),
      lags = 1,
      signals = c("y")
    )
  expect_is(result, "data.frame")
  expect_equal(ncol(result), 5)
  expect_error(process_dataframe(
    df,
    excluded_cols = c("a", "b"),
    lags = 1,
    signals = c("y")
  ))
})
