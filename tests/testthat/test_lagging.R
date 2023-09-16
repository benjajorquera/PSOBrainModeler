context("Test Lagging")

# Define Global Variables
base_df <-
  data.frame(A = c(1, 2, 3),
             B = c(4, 5, 6),
             C = c(7, 8, 9))
norm_colnames <- c("A_norm", "B_norm", "C_norm")
empty_df <- data.frame()

# ------------------------------------------------------------
# Tests for Lagging Functions
# ------------------------------------------------------------

# Test lag_normalized_signals
test_that("lag_normalized_signals adds lagged normalized columns to a dataframe",
          {
            norm_df <- normalize_all_signals(base_df)
            lagged_normalized_df <-
              lag_normalized_signals(norm_df, 2, c("A_norm", "B_norm"))
            expect_equal(
              colnames(lagged_normalized_df),
              c(
                "A",
                "B",
                "C",
                norm_colnames,
                "A_norm_1",
                "A_norm_2",
                "B_norm_1",
                "B_norm_2"
              )
            )
            expect_error(
              lag_normalized_signals(norm_df, 2, c("Z_norm")),
              "Some specified column names are not in the data frame."
            )
          })

# Test lag_all_signals
test_that("lag_all_signals adds lagged columns to all normalized signals in a dataframe",
          {
            norm_df <- normalize_all_signals(base_df)
            lagged_all_df <- lag_all_signals(norm_df, 2)
            expect_equal(
              colnames(lagged_all_df),
              c(
                "A",
                "B",
                "C",
                norm_colnames,
                "A_norm_1",
                "A_norm_2",
                "B_norm_1",
                "B_norm_2",
                "C_norm_1",
                "C_norm_2"
              )
            )
          })