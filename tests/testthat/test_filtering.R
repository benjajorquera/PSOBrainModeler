# Load Required Libraries
library(testthat)
library(dplyr)

# Source Utility Functions
source("Filtering.R")

# Define Global Variables
base_df <-
  data.frame(A = c(1, 2, 3),
             B = c(4, 5, 6),
             C = c(7, 8, 9))
norm_colnames <- c("A_norm", "B_norm", "C_norm")
empty_df <- data.frame()

# ------------------------------------------------------------
# Tests for Filtering Functions
# ------------------------------------------------------------

# Test filter_signals_dataframe
test_that("filter_signals_dataframe works as expected", {
  df <- data.frame(x = 1:5, y = 6:10, z = 11:15)
  result <- filter_signals_dataframe(df, signal_names = c("x", "y"))
  expect_is(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_error(filter_signals_dataframe(df, signal_names = c("a", "b")))
})

# Test exclude_signals_dataframe
test_that("exclude_signals_dataframe works as expected", {
  df <- data.frame(x = 1:5, y = 6:10, z = 11:15)
  result <-
    exclude_signals_dataframe(df, signal_names = c("x", "y"))
  expect_is(result, "data.frame")
  expect_equal(ncol(result), 1)
  expect_error(exclude_signals_dataframe(df, signal_names = c("a", "b")))
})