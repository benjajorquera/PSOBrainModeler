#' PSO Objective Function
#'
#' This function calculates an optimization objective for a Particle Swarm Optimization (PSO) algorithm.
#'
#' @param params A numeric vector containing optimization parameters:
#'   - cost: Cost parameter (rounded to 2 decimals).
#'   - nu: Nu parameter (rounded to 2 decimals).
#'   - gamma: Gamma parameter (rounded to 2 decimals if >= 1, else rounded to 2 significant digits).
#'
#' @return The optimization value for minimization.
#'

pso_objective <- function(params) {
  # Extract optimization parameters
  cost <- round(params[1], digits = 2)
  nu <- round(params[2], digits = 2)
  gamma <- params[3]
  if (gamma >= 1)
    gamma <- round(gamma, digits = 2)
  else
    gamma <- signif(gamma, digits = 2)
  
  lags <- round(params[4], digits = 0)
  
  # Print hyperparameters
  cat(cost, nu, gamma, lags, "\n")
  
  # Initialize vectors for correlations and errors
  cors <- numeric(BCV_FOLDS)
  errors <- numeric(BCV_FOLDS)
  
  # Iterate through data partitions
  for (df_list in 1:BCV_FOLDS) {
    # Prepare data
    new_data_validation <-
      generate_model_data(
        data_partitions[[df_list]]$validation,
        c("MABP_norm", "CBFV.L_norm"),
        c("MABP_norm"),
        lags,
        FALSE
      )
    
    data_partitions_training <-
      generate_model_data(
        data_partitions[[df_list]]$training,
        c("MABP_norm", "CBFV.L_norm"),
        c("MABP_norm"),
        lags,
        TRUE
      )
    
    # Train SVR model
    svr_model_pso <-
      vsvr_model(
        data_partitions_training,
        VSVR_RESPONSE,
        cost,
        nu,
        gamma,
        VSVR_TOLERANCE,
        VSVR_KERNEL
      )
    
    # Make predictions
    predictions_pso <- predict(svr_model_pso, new_data_validation)
    
    # Compute and save correlation
    cors[df_list] <-
      cor(predictions_pso,
          data_partitions[[df_list]]$validation$CBFV.L_norm)
    
    if (is.na(cors[df_list])) {
      print("NA CORRELATION: SVM TOLERANCE TOO HIGH")
      return(-1)
    }
    
    # Compute and save MSE
    errors[df_list] <-
      sqrt(mean((
        data_partitions[[df_list]]$validation$CBFV.L_norm - predictions_pso
      ) ^ 2
      ))
  }
  
  # Train model with all data
  data_training <- generate_model_data(data,
                                       c("MABP_norm", "CBFV.L_norm"),
                                       c("MABP_norm"),
                                       lags,
                                       TRUE)
  data_model <-
    vsvr_model(data_training,
               VSVR_RESPONSE,
               cost,
               nu,
               gamma,
               VSVR_TOLERANCE,
               VSVR_KERNEL)
  
  # Make response predictions
  CBFV_predictions <-
    generate_signal_response_predictions(
      data_model,
      pressure_df,
      PRESSURE_SIGNAL_START,
      PRESSURE_SIGNAL_RESPONSE_SIZE,
      c("MABP_norm", "CBFV.L_norm"),
      lags,
      c("MABP_norm"),
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
  
  # Print optimization values
  cat(mean(cors),
      mean(errors),
      (1 - mean(cors) + mean(errors)),
      (signal_score * 0.1),
      "\n")
  
  # Return optimization value for minimization
  return(2 - mean(cors) + mean(errors) - (signal_score * 0.1))
}
