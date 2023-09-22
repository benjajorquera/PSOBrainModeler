context("Tests for vsvr_model function")

test_that("vsvr_model works with different kernels", {
  # Example data
  data <-
    data.frame(x = rnorm(100),
               y = rnorm(100),
               target = rnorm(100))
  
  # Test for a linear kernel
  model_linear <-
    vsvr_model(data,
               "target",
               cost = 1,
               nu = 0.5,
               tolerance = 0.001)
  expect_is(model_linear, "svm")
  
  # Test for a radial kernel
  model_radial <-
    vsvr_model(
      data,
      "target",
      cost = 1,
      nu = 0.5,
      gamma = 0.01,
      tolerance = 0.001
    )
  expect_is(model_radial, "svm")
  
  # ------------------------------------------------------------
  # Tests for data validation
  # ------------------------------------------------------------
  
  # 1. Validate data type
  test_that("Error when 'data' is not a data.frame", {
    expect_error(vsvr_model(matrix(1:4, 2), "y", 1, 0.5, 0.01, 0.001))
  })
  
  # 2. Validate response_var
  test_that("Error when 'response_var' is not a single character string", {
    expect_error(vsvr_model(data, c("y", "x"), 1, 0.5, 0.01, 0.001))
    expect_error(vsvr_model(data, TRUE, 1, 0.5, 0.01, 0.001))
  })
  
  test_that("Error when response variable not found in data", {
    expect_error(vsvr_model(data, "z", 1, 0.5, 0.01, 0.001))
  })
  
  # 3. Validate cost, nu, and tolerance
  test_that("Error when 'cost', 'nu', or 'tolerance' are not numeric or out of range",
            {
              expect_error(vsvr_model(data, "y", "a", 0.5, 0.01, 0.001))
              expect_error(vsvr_model(data, "y", 1, "b", 0.01, 0.001))
              expect_error(vsvr_model(data, "y", 1, 0.5, 0.01, "c"))
              
              expect_error(vsvr_model(data, "y", 1,-0.5, 0.01, 0.001))
              expect_error(vsvr_model(data, "y", 1, 1.5, 0.01, 0.001))
            })
  
  # 4. Validate gamma
  test_that("Error when 'gamma' is not numeric or NULL", {
    expect_error(vsvr_model(data, "y", 1, 0.5, "a", 0.001))
  })
  
})
