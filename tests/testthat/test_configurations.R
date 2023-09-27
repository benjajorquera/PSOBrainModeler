context("Tests for Configurations")

# Test default values
test_that("configure_pso_brain_modeler returns default values correctly",
          {
            config <- configure_pso_brain_modeler()
            
            expect_equal(config$seed, 123)
            expect_equal(config$bcv_folds, 5)
            expect_equal(config$bcv_validation_size, 0.2)
            expect_equal(config$pressure_signal_start, 3)
            expect_equal(config$pressure_signal_response_size, 40)
            expect_equal(config$butter_filter_order, 2)
            expect_equal(config$butter_filter_fs, 0.2)
            expect_equal(config$max_lag_number, 8)
            expect_equal(config$vsvr_tolerance, 1)
          })

# Test the class of the returned object
test_that("configure_pso_brain_modeler returns an object of class 'PSOBrainModelerConfig'",
          {
            config <- configure_pso_brain_modeler()
            expect_equal(class(config), "PSOBrainModelerConfig")
          })

# Test custom values
test_that("configure_pso_brain_modeler sets custom values correctly", {
  custom_seed <- 456
  custom_bcv_folds <- 10
  custom_bcv_validation_size <- 0.3
  custom_pressure_signal_start <- 4
  custom_pressure_signal_response_size <- 50
  custom_butter_filter_order <- 3
  custom_butter_filter_fs <- 0.3
  custom_max_lag_number <- 9
  custom_vsvr_tolerance <- 2
  
  config <- configure_pso_brain_modeler(
    seed = custom_seed,
    bcv_folds = custom_bcv_folds,
    bcv_validation_size = custom_bcv_validation_size,
    pressure_signal_start = custom_pressure_signal_start,
    pressure_signal_response_size = custom_pressure_signal_response_size,
    butter_filter_order = custom_butter_filter_order,
    butter_filter_fs = custom_butter_filter_fs,
    max_lag_number = custom_max_lag_number,
    vsvr_tolerance = custom_vsvr_tolerance
  )
  
  expect_equal(config$seed, custom_seed)
  expect_equal(config$bcv_folds, custom_bcv_folds)
  expect_equal(config$bcv_validation_size, custom_bcv_validation_size)
  expect_equal(config$pressure_signal_start,
               custom_pressure_signal_start)
  expect_equal(config$pressure_signal_response_size,
               custom_pressure_signal_response_size)
  expect_equal(config$butter_filter_order, custom_butter_filter_order)
  expect_equal(config$butter_filter_fs, custom_butter_filter_fs)
  expect_equal(config$max_lag_number, custom_max_lag_number)
  expect_equal(config$vsvr_tolerance, custom_vsvr_tolerance)
})

# Test default values
test_that("configure_psoptim_control sets default values correctly", {
  config <- configure_psoptim_control()
  
  expect_equal(config$trace, 1)
  expect_equal(config$fnscale, 1)
  expect_equal(config$maxit, 100)
  expect_equal(config$maxf, 200)
  expect_equal(config$abstol, -Inf)
  expect_equal(config$reltol, 0.5)
  expect_equal(config$REPORT, 1)
  expect_equal(config$trace.stats, TRUE)
  expect_equal(config$s, 5)
  expect_equal(config$k, 3)
  expect_equal(config$p, 0.5)
  expect_equal(config$w, (1 / (2 * log(2))))
  expect_equal(config$c.p, 5 + log(2))
  expect_equal(config$c.g, 10)
  expect_equal(config$rand.order, TRUE)
  expect_equal(config$max.restart, Inf)
  expect_equal(config$maxit.stagnate, 50)
  expect_equal(config$vectorize, TRUE)
  expect_equal(config$hybrid, "improved")
  expect_equal(config$type, "SPSO2011")
  expect_equal(config$hybrid.control, list(maxit = 1))
})

# Test custom values
test_that("configure_psoptim_control sets custom values correctly", {
  custom_config <- configure_psoptim_control(
    pso_trace = 2,
    pso_fnscale = 2,
    pso_max_iterations = 150,
    pso_max_fn_calls = 250,
    pso_abstol = -10,
    pso_restart_tolerance = 0.8,
    pso_report = 2,
    pso_trace_stats = FALSE,
    pso_swarm_size = 6,
    pso_informants = 4,
    pso_informed_swarm = 0.6,
    pso_exploitation_const = 1,
    pso_local_exp_const = 6,
    pso_global_exp_const = 11,
    pso_rand_order = FALSE,
    pso_max_restart = 10,
    pso_maxit_without_improvement = 60,
    pso_hybrid_type = "custom",
    pso_type = "SPSO2020",
    pso_vectorization = FALSE,
    pso_hybrid_control = list(maxit = 2)
  )
  
  expect_equal(custom_config$trace, 2)
  expect_equal(custom_config$fnscale, 2)
  expect_equal(custom_config$maxit, 150)
  expect_equal(custom_config$maxf, 250)
  expect_equal(custom_config$abstol, -10)
  expect_equal(custom_config$reltol, 0.8)
  expect_equal(custom_config$REPORT, 2)
  expect_equal(custom_config$trace.stats, FALSE)
  expect_equal(custom_config$s, 6)
  expect_equal(custom_config$k, 4)
  expect_equal(custom_config$p, 0.6)
  expect_equal(custom_config$w, 1)
  expect_equal(custom_config$c.p, 6)
  expect_equal(custom_config$c.g, 11)
  expect_equal(custom_config$rand.order, FALSE)
  expect_equal(custom_config$max.restart, 10)
  expect_equal(custom_config$maxit.stagnate, 60)
  expect_equal(custom_config$vectorize, FALSE)
  expect_equal(custom_config$hybrid, "custom")
  expect_equal(custom_config$type, "SPSO2020")
  expect_equal(custom_config$hybrid.control, list(maxit = 2))
})

# Test the class of the returned object
test_that(
  "configure_psoptim_control returns an object of class 'PSOBrainModelerPSOPTIMConfig'",
  {
    config <- configure_psoptim_control()
    expect_equal(class(config), "PSOBrainModelerPSOPTIMConfig")
  }
)

# Tests for configure_data_env
test_that("configure_data_env returns a list of processed data", {
  config <- configure_pso_brain_modeler()
  data <- data.frame(feature1 = rnorm(10), feature2 = rnorm(10))
  data_env_config <-
    configure_data_env(
      config = config,
      data = data,
      excluded_cols = NULL,
      signal_names = c("feature1", "feature2"),
      predictors_names = c("feature1"),
      vsvr_response = "feature2"
    )
  expect_is(data_env_config, "list")
  expect_is(data_env_config$pressure_df, "data.frame")
  expect_is(data_env_config$processed_data, "data.frame")
  expect_is(data_env_config$data_partitions, "list")
  expect_equal(data_env_config$NORM_SIGNAL_NAMES,
               c("feature1_norm", "feature2_norm"))
  expect_equal(data_env_config$NORM_PREDICTORS_NAMES, c("feature1_norm"))
  expect_equal(data_env_config$NORM_VSVR_RESPONSE, "feature2_norm")
  expect_equal(data_env_config$VSVR_TOL, 1)
})