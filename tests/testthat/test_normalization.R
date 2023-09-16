context("Test Normalization")

# Define Global Variables
base_df <-
  data.frame(A = c(1, 2, 3),
             B = c(4, 5, 6),
             C = c(7, 8, 9))
norm_colnames <- c("A_norm", "B_norm", "C_norm")
empty_df <- data.frame()

# ------------------------------------------------------------
# Tests for Normalization Functions
# ------------------------------------------------------------

# Test normalize_all_signals
test_that("normalize_all_signals normalizes all signals of a dataframe", {
  normalized_df <- normalize_all_signals(base_df)
  expect_equal(colnames(normalized_df), c(colnames(base_df), norm_colnames))
  expect_error(normalize_all_signals(empty_df),
               "Input data frame should not be empty or NULL.")
})

# Test normalize_signals_by_name
test_that("normalize_signals_by_name normalizes given signals of a dataframe",
          {
            normalized_df <- normalize_signals_by_name(base_df, c("A", "B"))
            expect_equal(colnames(normalized_df), c(colnames(base_df), "A_norm", "B_norm"))
            expect_error(
              normalize_signals_by_name(base_df, c("Z")),
              "Some specified signal names are not in the data frame."
            )
            expect_error(
              normalize_signals_by_name(empty_df, c("A")),
              "Input data frame should not be empty or NULL."
            )
          })