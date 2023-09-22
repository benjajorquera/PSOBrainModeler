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

#' PSO Objective Function for Different Models
#'
#' @param params A numeric vector containing optimization parameters.
#' @param model A character string indicating the model to optimize.
#'
#' @return The optimization value for minimization.
main_pso_objective <- function(params, model) {
  print(model)
  switch(
    model,
    "FIR" = {
      params_list <- extract_and_round_params(params)
      cat(params_list$cost, params_list$nu, params_list$lags, "\n")
      return(pso_training_model(params_list$cost, params_list$nu, NULL, params_list$lags))
    },
    "NFIR" = {
      params_list <- extract_and_round_params(params, has_gamma = TRUE)
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
    "ARX" = {
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
    "NARX" = {
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

pso_optim <- function(params, model) {
  return(
    psoptim(
      par = HYPER_PARAMS_INITIAL_VALUES,
      fn = main_pso_objective(params, model),
      lower = HYPER_PARAMS_LOWER_BOUNDS,
      upper = HYPER_PARAMS_UPPER_BOUNDS,
      control = list(
        s = PSO_SWARM_SIZE,
        maxit = PSO_MAX_ITERATIONS,
        maxf = PSO_MAX_FUNCTION_CALLS,
        trace = 1,
        REPORT = 1,
        p = PSO_AVG_INFORMED_PARTICLES,
        c.g = PSO_GLOBAL_EXPLORATION_CONST,
        trace.stats = TRUE,
        maxit.stagnate = PSO_MAX_IT_WITHOUT_IMPROVEMENT,
        reltol = PSO_RESTART_TOLERANCE,
        hybrid = PSO_HYBRID_TYPE,
        type = PSO_TYPE,
        vectorize = PSO_VECTORIZATION,
        hybrid.control = PSO_HYBRID_CONTROL
      )
    )
  )
}

cross_validate_partition <- function(cost, nu, gamma) {
  cors <- numeric(BCV_FOLDS)
  errors <- numeric(BCV_FOLDS)
  
  for (df_list in 1:BCV_FOLDS) {
    # Prepare data
    new_data_validation <- generate_time_series_data(
      data_partitions[[df_list]]$validation,
      SIGNAL_NORM_NAMES,
      PREDICTORS_NORM_NAMES,
      LAGGED_COLS,
      COL_LAGS,
      FALSE
    )
    
    data_partitions_training <- generate_time_series_data(
      data_partitions[[df_list]]$training,
      SIGNAL_NORM_NAMES,
      PREDICTORS_NORM_NAMES,
      LAGGED_COLS,
      COL_LAGS,
      TRUE
    )
    
    # Train SVR model
    svr_model_pso <-
      vsvr_model(data_partitions_training,
                 VSVR_RESPONSE,
                 cost,
                 nu,
                 gamma,
                 VSVR_TOLERANCE)
    
    # Make predictions
    predictions_pso <- predict(svr_model_pso, new_data_validation)
    
    if (sd(predictions_pso) == 0) {
      message(
        "STANDARD DEVIATION OF 'PREDICTIONS PSO' IS ZERO: SVM TOLERANCE IS TOO HIGH FOR NUMBER OF LAGS USED"
      )
      return(list(avg_cor = NA, avg_error = NA))
    }
    
    # Compute and save correlation
    cors[df_list] <-
      cor(predictions_pso,
          data_partitions[[df_list]]$validation$CBFV.L_norm)
    
    # Compute and save MSE
    errors[df_list] <-
      sqrt(mean((
        data_partitions[[df_list]]$validation$CBFV.L_norm - predictions_pso
      ) ^ 2
      ))
  }
  
  avg_cor <- mean(cors, na.rm = TRUE)
  avg_error <- mean(errors, na.rm = TRUE)
  
  return(list(avg_cor = avg_cor, avg_error = avg_error))
}

pso_training_model <-
  function(cost,
           nu,
           gamma = NULL,
           COL_LAGS,
           RESPONSE_LAGS = NULL) {
    results <- cross_validate_partition(cost, nu, gamma)
    
    # Acceder a la correlación promedio y al error
    avg_cor <- results$avg_cor
    avg_error <- results$avg_error
    
    # Make response predictions
    CBFV_predictions <-
      generate_signal_response_predictions(
        data_model,
        pressure_df,
        PRESSURE_SIGNAL_START,
        PRESSURE_SIGNAL_RESPONSE_SIZE,
        SIGNAL_NORM_NAMES,
        COL_LAGS,
        RESPONSE_LAGS,
        PREDICTORS_NORM_NAMES,
        c(1),
        VSVR_RESPONSE,
        0.8
      )
    
    # Process response signal and score it
    signal_score <-
      process_signal(CBFV_predictions$CBFV.L_norm, PRESSURE_SIGNAL_START)
    
    # Plot response signal if it passes a basic filter
    if (signal_score > 0) {
      plot(CBFV_predictions$CBFV.L_norm, type = "l")
    }
    else {
      return(5)
    }
    
    # Print optimization values
    cat(mean(cors),
        mean(errors),
        (1 - mean(cors) + mean(errors)),
        (signal_score * 0.1),
        "\n")
    
    # Return optimization value for minimization
    return(2 - mean(cors) + mean(errors) - (signal_score * 0.1))
  }

