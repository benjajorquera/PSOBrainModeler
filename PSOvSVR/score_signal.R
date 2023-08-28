generate_CBFV_predictions <-
  function(pressure_start,
           initial_column_values,
           predictions_size,
           #SVR_model,
           column_names,
           columns_lags,
           initial_column_names,
           prediction_col_name) {
    # if ((length(initial_column_names) == 0) ||
    #     (length(initial_column_values) == 0) ||
    #     ((length(initial_column_values)) != (length(initial_column_names)))) {
    #   print("Empty or different sizes of colummns name and values")
    #   return()
    # }
    
    initial_values <-
      setNames(initial_column_values, column_names)
    
    pressure_start <- 5
    pressure_df <- add_pressure_step(pressure_start)
    
    pressure_df_model <-
      setNames(data.frame(name = rep(
        initial_column_values[1], pressure_start
      )),
      initial_column_names[1])
    
    if (length(initial_column_names) > 1) {
      for (i in 2:length(initial_column_names)) {
        pressure_df_model <-
          cbind(pressure_df_model,
                setNames(
                  data.frame(name = initial_column_values[i]),
                  initial_column_names[i]
                ))
      }
    }
    
    
    for (col_name in column_names) {
      for (lag in 1:columns_lags) {
        new_col_name <- paste0(col_name, "_", lag)
        new_col_values <-
          rep(unname(initial_values[col_name]), pressure_start)
        pressure_df_model <-
          cbind(pressure_df_model, setNames(data.frame(name = new_col_values),
                                            new_col_name))
      }
    }

    print(pressure_df_model)
    
    for(i in initial_column_names) {
      new_pressure <- setNames(data.frame(name = pressure_df[(pressure_start + 1),]), i)
    }
    
    #predictions_data_pressure <-
    #  predict(SVR_model, pressure_df_model)
    
    predictions_data_pressure <- c(0.5, 0.5, 0.5)
    new_pressure <-
      cbind(new_pressure, setNames(data.frame(name = tail(
        predictions_data_pressure, 1
      )), paste0(prediction_col_name, "_1")))
    
    
    print(new_pressure)
    
    # for (i in (pressure_start + 1):predictions_size){
    #   predictions_data_pressure <- predict(SVR_model, pressure_df_model)
    # 
    #   pressure_df_model_predictions <- data.frame(pressure_df[initial_column_names])
    # 
    #   for (col_name in column_names) {
    #     for (lag in 2:columns_lags) {
    #       new_col_name <- paste0(col_name, "_", lag)
    #       col_data <- paste0(col_name, "_", (lag-1))
    #       #new_col_values <-
    #       #  rep(unname(initial_values[col_name]), pressure_start)
    #       new_col_values <- tail(pressure_df_model[col_data], 1)
    #       pressure_df_model <-
    #         cbind(pressure_df_model, setNames(data.frame(name = new_col_values),
    #                                           new_col_name))
    #     }
    #   }
    # }


    # for (i in (pressure_start + 1):predictions_size) {
    #   predictions_data_pressure <- predict(SVR_model, pressure_df_model)
    #   new_pressure <- data.frame(
    #     MABP_norm = pressure_df$MABP_norm[i],
    #     MABP_norm_1 = tail(pressure_df_model$MABP_norm, 1),
    #     MABP_norm_2 = tail(pressure_df_model$MABP_norm_1, 1),
    #     MABP_norm_3 = tail(pressure_df_model$MABP_norm_2, 1),
    #     CBFV.L_norm_1 = tail(predictions_data_pressure, 1),
    #     CBFV.L_norm_2 = tail(pressure_df_model$CBFV.L_norm_1, 1),
    #     CBFV.L_norm_3 = tail(pressure_df_model$CBFV.L_norm_2, 1)
    #   )
    #   pressure_df_model <- rbind(pressure_df_model, new_pressure)
    #   row.names(pressure_df_model) <- NULL
    # }
    # return(pressure_df_model)
  }


process_signal <- function(signal, pressure_start) {
  response_signal <- signal[pressure_start:(pressure_start + 30)]
  peak_signal <- signal[(pressure_start + 6):(pressure_start + 18)]
  stable_signal <- signal[(pressure_start + 30):(pressure_start + 60)]
  min_response_signal <- min(response_signal)
  
  if (!(min_response_signal %in% peak_signal) ||
      !(min_response_signal >= -.2 &&
        min_response_signal <= .5) ||
      var(stable_signal) > .002 ||
      !(max(signal) < 1.2)) {
    return(0)
  }
  
  score <- 10
  
  max_peak_signal <-
    max(signal[(pressure_start + 18):(pressure_start + 30)])
  peak_stable_distance <-
    abs(max_peak_signal - tail(stable_signal, 1))
  score <- score - (peak_stable_distance * 100)
  
  stable_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - tail(stable_signal, 1)
  drop_peak <-
    max(signal[(pressure_start + 18):(pressure_start + 30)]) - min(peak_signal)
  
  if (stable_peak > (drop_peak * .45)) {
    score <- score - (drop_peak * 10)
  }
  
  if (min(stable_signal) < min_response_signal) {
    score <- score - ((min_response_signal - min(stable_signal)) * 10)
  }
  
  return(score)
}