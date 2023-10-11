svr_grid_search <- function(config,
                            data,
                            model,
                            multi = FALSE,
                            signal_names,
                            excluded_cols = NULL,
                            predictors_names,
                            params_lower_bounds,
                            params_upper_bounds,
                            vsvr_response,
                            silent = FALSE,
                            plot_response = TRUE,
                            initial_pressure_value = c(1)) {
  validate_inputs_main(
    data,
    model,
    signal_names,
    predictors_names,
    vsvr_response,
    excluded_cols,
    multi,
    silent,
    params_lower_bounds,
    params_upper_bounds
  )
  
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
  
  # Cross-validation
  results <- cross_validate_partition_helper(
    cost = params_upper_bounds[1],
    nu = params_upper_bounds[2],
    gamma = NULL,
    col_lags = c(8),
    data_list = data_env_list,
    silent = silent,
    search_method = 'Grid'
  )
  
  print(results)
  
  
  return(data_env_list)
}