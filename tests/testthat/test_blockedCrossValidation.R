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


test_that("cross_validate_partition data correctly", {
  data_partitions_test <- list(
    list(
      training = data.frame(
        feature1 = rnorm(20),
        feature2 = rnorm(20),
        feature1_1 = rnorm(20)
      ),
      validation = data.frame(
        feature1 = rnorm(20),
        feature2 = rnorm(20),
        feature1_1 = rnorm(20)
      )
    ),
    list(
      training = data.frame(
        feature1 = rnorm(20),
        feature2 = rnorm(20),
        feature1_1 = rnorm(20)
      ),
      validation = data.frame(
        feature1 = rnorm(20),
        feature2 = rnorm(20),
        feature1_1 = rnorm(20)
      )
    )
  )
  result <- cross_validate_partition(
    cost = 1,
    nu = 0.5,
    gamma = NULL,
    data_partitions_test,
    bcv_folds = 2,
    signal_norm_names = c("feature1", "feature2"),
    predictors_norm_names = c("feature1"),
    lagged_cols = c("feature1"),
    col_lags = c(1),
    vsvr_response = "feature2",
    vsvr_tolerance = 1
  )
  
  # Check that the result is a list
  expect_true(is.list(result))
  
  # Check that the list has a length of 2
  expect_length(result, 2)
  
  # Check that the list contains the keys 'avg_cor' and 'avg_error'
  expect_true("avg_cor" %in% names(result))
  expect_true("avg_error" %in% names(result))
  
  # Check that 'avg_cor' and 'avg_error' are of numeric type
  expect_true(is.numeric(result$avg_cor))
  expect_true(is.numeric(result$avg_error))
  
  # Validations for 'avg_cor'
  expect_true(result$avg_cor >= -1, info = "avg_cor is less than -1")
  expect_true(result$avg_cor <= 1, info = "avg_cor is greater than 1")
  
  # Validations for 'avg_error'
  expect_true(result$avg_error >= 0, info = "avg_error is negative")
  
})

# Mock data and arguments for the tests
mock_data_partitions <-
  list(list(
    training = data.frame(
      feature1 = rnorm(20),
      feature2 = rnorm(20),
      feature1_1 = rnorm(20)
    ),
    validation = data.frame(
      feature1 = rnorm(20),
      feature2 = rnorm(20),
      feature1_1 = rnorm(20)
    )
  ))

# Test for correct argument types and values
test_that("cross_validate_partition handles invalid arguments", {
  expect_error(
    cross_validate_partition(
      cost = "wrong_type",
      nu = 0.5,
      gamma = NULL,
      mock_data_partitions,
      1,
      c("feature1", "feature2"),
      c("feature1"),
      c("feature1"),
      c(1),
      "feature2",
      1
    ),
    "The 'cost' argument must be numeric."
  )
  
  expect_error(
    cross_validate_partition(
      cost = 1,
      nu = "wrong_type",
      gamma = NULL,
      mock_data_partitions,
      1,
      c("feature1", "feature2"),
      c("feature1"),
      c("feature1"),
      c(1),
      "feature2",
      1
    ),
    "The 'nu' argument must be numeric."
  )
  
  expect_error(
    cross_validate_partition(
      cost = 1,
      nu = 0.5,
      gamma = "wrong_type",
      mock_data_partitions,
      1,
      c("feature1", "feature2"),
      c("feature1"),
      c("feature1"),
      c(1),
      "feature2",
      1
    ),
    "The 'gamma' argument must be numeric or NULL."
  )
  
  expect_error(
    cross_validate_partition(
      cost = 1,
      nu = 0.5,
      gamma = NULL,
      data_partitions = "wrong_type",
      bcv_folds = 1,
      signal_norm_names = c("feature1", "feature2"),
      predictors_norm_names = c("feature1"),
      lagged_cols = c("feature1"),
      col_lags = c(1),
      vsvr_response = "feature2",
      vsvr_tolerance = 1
    ),
    "The 'data_partitions' argument must be a list."
  )
  
  expect_error(
    cross_validate_partition(
      cost = 1,
      nu = 0.5,
      gamma = NULL,
      mock_data_partitions,
      bcv_folds = c(1, 2),
      signal_norm_names = c("feature1", "feature2"),
      predictors_norm_names = c("feature1"),
      lagged_cols = c("feature1"),
      col_lags = c(1),
      vsvr_response = "feature2",
      vsvr_tolerance = 1
    ),
    "The 'bcv_folds' argument must be a single integer."
  )
})