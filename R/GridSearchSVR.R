svr_grid_search <- function(config,
                            data,
                            model,
                            multi = FALSE,
                            signal_names,
                            excluded_cols = NULL,
                            predictors_names,
                            vsvr_response,
                            silent = FALSE,
                            plot_response = TRUE,
                            initial_pressure_value = c(1)) {
  data_env_list <- configure_data_env(
    config,
    data = data,
    signal_names = signal_names,
    excluded_cols = excluded_cols,
    predictors_names = predictors_names,
    vsvr_response = vsvr_response,
    multi = multi,
    extra_col_name = "etCO2",
    initial_prediction_values = initial_pressure_value
  )
  
  training_x <-
    exclude_signals_dataframe(data_env_list$processed_data,
                              data_env_list$NORM_VSVR_RESPONSE)
  training_y <-
    filter_signals_dataframe(data_env_list$processed_data,
                             data_env_list$NORM_VSVR_RESPONSE)
  
  if (model == 'FIR') {
    nu <- seq(0.1, 0.9, 0.1)
    cost <-
      c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
    params_ranges <- list(cost = cost, nu = nu)
    kernel <- 'linear'
  }
  if (model == 'NFIR') {
    nu <- seq(0.1, 0.9, 0.1)
    cost <-
      c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)
    sigma <- 2 ^ seq(-4, 10, 2)
    gamma <- 1 / (2 * sigma ^ 2)
    params_ranges <- list(cost = cost,
                          nu = nu,
                          gamma = gamma)
    kernel <- 'radial'
  }
  
  grid_search <- e1071::tune(
    METHOD = 'svm',
    train.x = training_x,
    train.y = training_y,
    ranges = params_ranges,
    type = "nu-regression",
    kernel = kernel,
    tune.control = list(
      random = FALSE,
      cross = 5,
      samping = 'cross',
      best.model = TRUE
    )
  )
  
  #### VARIAR PLIEGUES DE VALIDACIÓN CRUZADA EN PSO
  
  return(grid_search)
}