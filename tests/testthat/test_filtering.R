context("Test Filtering")

# Sample data frame
sample_df <- data.frame(A = c(1, 2, 3),
                        B = c(4, 5, 6),
                        C = c(7, 8, 9))

test_that("filter_signals_dataframe works correctly", {
  # 1. Test for successful filtering with valid signal names
  result_df <- filter_signals_dataframe(sample_df, c("A", "B"))
  expect_equal(dim(result_df)[2], 2)
  expect_true(all(colnames(result_df) == c("A", "B")))
  
  # 2. Test for successful filtering with one valid signal name
  result_df <- filter_signals_dataframe(sample_df, "A")
  expect_equal(dim(result_df)[2], 1)
  expect_true(all(colnames(result_df) == "A"))
  
  # 7. Test that the function doesn't inadvertently drop the dataframe structure when selecting a single column
  result_df <- filter_signals_dataframe(sample_df, "A")
  expect_is(result_df, "data.frame")
})

test_that("exclude_signals_dataframe works correctly", {
  # 1. Test for successful exclusion with valid signal names
  result_df <- exclude_signals_dataframe(sample_df, c("A", "B"))
  expect_equal(dim(result_df)[2], 1)
  expect_true(all(colnames(result_df) == "C"))
  
  # 2. Test for successful exclusion with one valid signal name
  result_df <- exclude_signals_dataframe(sample_df, "A")
  expect_equal(dim(result_df)[2], 2)
  expect_true(all(colnames(result_df) == c("B", "C")))

  # 6. Test for return type
  result_df <- exclude_signals_dataframe(sample_df, c("A", "B"))
  expect_is(result_df, "data.frame")
  
  # 7. Test that the function doesn't inadvertently drop the dataframe structure when excluding a single column
  result_df <- exclude_signals_dataframe(sample_df, "A")
  expect_is(result_df, "data.frame")
})