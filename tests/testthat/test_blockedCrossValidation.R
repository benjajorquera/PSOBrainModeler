context("Tests for blocked_cv function")

test_that("blocked_cv partitions data correctly", {
  # Example data
  data <- data.frame(x = 1:100, y = 101:200)
  
  # 1. Validate data type
  test_that("Error when 'data' is not a data.frame", {
    expect_error(blocked_cv(matrix(1:4, 2), 2, 0.5))
  })
  
  # 2. Validate num_blocks
  test_that("Error when 'num_blocks' is not a positive integer", {
    expect_error(blocked_cv(data, "a", 0.5))
    expect_error(blocked_cv(data,-2, 0.5))
    expect_error(blocked_cv(data, 2.5, 0.5))
  })
  
  # 3. Validate validation_size
  test_that("Error when 'validation_size' is not between 0 and 1", {
    expect_error(blocked_cv(data, 2, "a"))
    expect_error(blocked_cv(data, 2,-0.1))
    expect_error(blocked_cv(data, 2, 1.1))
  })
  
  # 4. Check if dataset can be partitioned based on provided parameters
  test_that(
    "Error when dataset cannot be partitioned based on 'num_blocks' and 'validation_size'",
    {
      expect_error(blocked_cv(data, 101, 0.01))
    }
  )
  
  # Create partitions
  partitions <-
    blocked_cv(data, num_blocks = 5, validation_size = 0.2)
  
  # Test number of partitions
  expect_equal(length(partitions), 5)
  
  # Test size of a validation set
  expect_equal(nrow(partitions[[1]]$validation), 20)
  
  # Test size of a training set
  expect_equal(nrow(partitions[[1]]$training), 80)
})
