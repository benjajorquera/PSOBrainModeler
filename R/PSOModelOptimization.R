#' Extract and Round Parameters
#'
#' @param params A numeric vector with the optimization parameters.
#' @param has_gamma A logical indicating if the gamma parameter is included.
#' @param n_lags The number of lags being optimized.
#'
#' @return A list containing the rounded parameters.
extract_and_round_params <-
  function(params,
           has_gamma = FALSE,
           n_lags = 1) {
    cost <- round(params[1], digits = 2)
    nu <- round(params[2], digits = 2)
    
    if (has_gamma) {
      gamma <- params[3]
      if (gamma >= 1)
        gamma <- round(gamma, digits = 2)
      else
        gamma <- signif(gamma, digits = 2)
      lags <- round(params[4:(3 + n_lags)])
      return(list(
        cost = cost,
        nu = nu,
        gamma = gamma,
        lags = lags
      ))
    } else {
      lags <- round(params[3:(2 + n_lags)])
      return(list(
        cost = cost,
        nu = nu,
        lags = lags
      ))
    }
  }

pso_test <- function(params) {
  params_list <-
    extract_and_round_params(params, has_gamma = FALSE, n_lags = 1)
  cat("Cost: ",
      params_list$cost,
      "Nu: ",
      params_list$nu,
      "Lags: ",
      params_list$lags,
      "\n")
  return(
    pso_training_model(
      cost = params_list$cost,
      nu = params_list$nu,
      NULL,
      col_lags = params_list$lags
    )
  )
}

#' PSO Objective Function for Different Models
#'
#' @param params A numeric vector containing optimization parameters.
#' @param model A character string indicating the model to optimize.
#'
#' @return The optimization value for minimization.
main_pso_objective <- function(model) {
  switch(
    model,
    "FIR" = {
      return(pso_test)
    },
    "2" = {
      message("NFIR")
      params_list <-
        extract_and_round_params(params, has_gamma = TRUE)
      cat(params_list$cost, params_list$nu, params_list$lags, "\n")
      return(
        pso_training_model(
          params_list$cost,
          params_list$nu,
          params_list$gamma,
          params_list$lags
        )
      )
    },
    "3" = {
      message("ARX")
      params_list <-
        extract_and_round_params(params, n_lags = 2)
      cat(params_list$cost,
          params_list$nu,
          NULL,
          params_list$lags,
          "\n")
      return(pso_training_model(params_list$cost,
                                params_list$nu,
                                NULL,
                                params_list$lags))
    },
    "4" = {
      message("NARX")
      params_list <-
        extract_and_round_params(params, has_gamma = TRUE, n_lags = 2)
      cat(params_list$cost, params_list$nu, params_list$lags, "\n")
      return(
        pso_training_model(
          params_list$cost,
          params_list$nu,
          params_list$gamma,
          params_list$lags
        )
      )
    }
  )
}

pso_optim <- function(seed = 123,
                      data,
                      model,
                      signal_names,
                      excluded_cols,
                      predictors_names,
                      params_lower_bounds,
                      params_upper_bounds,
                      params_initial_values,
                      bcv_folds = 5,
                      bcv_validation_size = 0.2,
                      pressure_signal_start = 3,
                      pressure_signal_response_size = 40,
                      butter_filter_order = 2,
                      butter_filter_fs = 0.2,
                      max_lag_number = 8,
                      vsvr_tolerance = 1,
                      vsvr_response,
                      pso_swarm_size = 5,
                      pso_max_iterations = 100,
                      pso_max_fn_calls = 200,
                      pso_informed_swarm = 0.5,
                      pso_global_exp_const = 10,
                      pso_maxit_without_improvement = 50,
                      pso_restart_tolerance = 0.5,
                      pso_hybrid_type = "improved",
                      pso_type = "SPSO2011",
                      pso_vectorization = TRUE,
                      pso_hybrid_control = list(maxit = 1)) {
  set.seed(seed)
  
  # Generate a data frame for the smoothed negative pressure step response with a Butterworth filter
  pressure_df <<-
    add_pressure_step(
      pressure_start = pressure_signal_start,
      signal_end = pressure_signal_response_size,
      butter_order = butter_filter_order,
      butter_fs = butter_filter_fs
    )
  
  
  processed_data <<-
    process_dataframe(
      df = data,
      excluded_cols = excluded_cols,
      lags = max_lag_number,
      signals = signal_names,
      lagged_signals = predictors_names
    )
  
  # Perform k-folded blocked cross-validation
  data_partitions <<-
    blocked_cv(data = processed_data,
               num_blocks = bcv_folds,
               validation_size = bcv_validation_size)
  
  NORM_SIGNAL_NAMES <<- paste0(signal_names, "_norm")
  NORM_PREDICTORS_NAMES <<- paste0(predictors_names, "_norm")
  NORM_VSVR_RESPONSE <<- paste0(vsvr_response, "_norm")
  VSVR_TOL <<- vsvr_tolerance
  
  main_pso <- main_pso_objective(model)
  
  return(
    psoptim(
      par = params_initial_values,
      fn = main_pso,
      lower = params_lower_bounds,
      upper = params_upper_bounds,
      control = list(
        s = pso_swarm_size,
        maxit = pso_max_iterations,
        maxf = pso_max_fn_calls,
        trace = 1,
        REPORT = 1,
        p = pso_informed_swarm,
        c.g = pso_global_exp_const,
        trace.stats = TRUE,
        maxit.stagnate = pso_maxit_without_improvement,
        reltol = pso_restart_tolerance,
        hybrid = pso_hybrid_type,
        type = pso_type,
        vectorize = pso_vectorization,
        hybrid.control = pso_hybrid_control
      )
    )
  )
  
}

pso_training_model <-
  function(cost,
           nu,
           gamma = NULL,
           col_lags,
           response_lags = NULL) {
    results <-
      cross_validate_partition(
        cost = cost,
        nu = nu,
        gamma = gamma,
        data_partitions = data_partitions,
        signal_norm_names = NORM_SIGNAL_NAMES,
        predictors_norm_names = NORM_PREDICTORS_NAMES,
        lagged_cols = NORM_PREDICTORS_NAMES,
        col_lags = col_lags,
        vsvr_response = NORM_VSVR_RESPONSE
      )
    
    # acceder a la correlación promedio y al error
    avg_cor <- results$avg_cor
    avg_error <- results$avg_error
    
    if (is.na(avg_cor) || is.na(avg_error))
      return (10)
    
    # train model with all data
    data_training <- generate_time_series_data(
      input_df = processed_data,
      data_cols = NORM_SIGNAL_NAMES,
      predictor_cols = NORM_PREDICTORS_NAMES,
      lagged_cols = NORM_PREDICTORS_NAMES,
      lag_values = col_lags,
      is_training = TRUE
    )
    
    # Make response predictions
    response_predictions <-
      generate_signal_response_predictions(
        data = data_training,
        pressure_signal_df = pressure_df,
        column_names = NORM_SIGNAL_NAMES,
        initial_columns_lags = col_lags,
        predicted_column_lags = response_lags,
        initial_column_names = NORM_PREDICTORS_NAMES,
        initial_column_values = c(1),
        prediction_col_name = NORM_VSVR_RESPONSE,
        prediction_initial_value = 0.8,
        cost = cost,
        nu = nu,
        gamma = gamma,
        tolerance = VSVR_TOL
      )
    
    # process response signal and score it
    signal_score <-
      evaluate_signal_quality(response_predictions[[NORM_VSVR_RESPONSE]])
    
    # plot response signal if it passes a basic filter
    if (signal_score > 0) {
      plot(response_predictions[[NORM_VSVR_RESPONSE]], type = "l")
    }
    else {
      return(5)
    }
    
    # print optimization values
    cat("AVG COR: ",
        avg_cor,
        "AVG MSE: ",
        avg_error,
        "Signal score: ",
        signal_score,
        "\n")
    
    # return optimization value for minimization
    return(2 - avg_cor + avg_error - (signal_score * 0.1))
  }





source("R/TimeSeries.R")
source("R/Filtering.R")
source("R/Normalization.R")
source("R/Lagging.R")
source("R/BlockedCrossValidation.R")
source("R/VSVMRegression.R")
source("R/TimeSeriesSignalPrediction.R")
source("R/ScoreSignal.R")
library(dplyr)
library(pso)
mydata <- read.table("data-raw/Sujeto1.txt", header = TRUE)

message("FIR")
result <- pso_optim(
  data = mydata,
  model = "FIR",
  signal_names = c("MABP", "CBFV.L"),
  excluded_cols = c("Time", "CBFV.R", "etCO2"),
  predictors_names = c("MABP"),
  params_lower_bounds = c(1, 0.1, 1),
  params_upper_bounds = c(10, 0.5, 3),
  params_initial_values = c(NA, NA, NA),
  vsvr_response = "CBFV.L",
  pressure_signal_start = 5
)
