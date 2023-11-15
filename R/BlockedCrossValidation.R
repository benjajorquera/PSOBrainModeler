#' Generate Data Partitions for Blocked K-fold Cross-Validation
#'
#' @param data Dataframe. The dataset to partition.
#' @param num_blocks (Optional) Integer. The number of blocks to divide the dataset into. Defaults to 5.
#' @param validation_size (Optional) Numeric (between 0 and 1). Proportion of data to be used
#'  for validation in each partition. Defaults to 0.2.
#'
#' @return A list of partitions, each containing a training set and a validation set.
#'
#' @details
#' The function employs several auxiliary validation functions to ensure inputs adhere to expected criteria:
#' - \code{\link{validate_data}}: Ensures provided data is non-empty dataframe.
#'
#' @examples
#' data <- data.frame(x = 1:100, y = 101:200)
#' partitions <- blocked_cv(data, num_blocks = 5, validation_size = 0.2)
#'
#' @export
#'
blocked_cv <-
  function(data,
           num_blocks = 5,
           validation_size = 0.2) {
    # Validation Checks
    validate_data(data)
    if (!is.numeric(num_blocks) ||
        (num_blocks < 1) ||
        (num_blocks != round(num_blocks)))
      stop("'num_blocks' must be a positive integer.")
    if (!is.numeric(validation_size) ||
        validation_size <= 0 ||
        validation_size >= 1)
      stop("'validation_size' must be a number between 0 and 1.")
    
    # Compute the validation length based on the total rows and desired size
    validation_length <- round(nrow(data) * validation_size)
    
    # Generate partitions
    data_partitions <- lapply(1:num_blocks, function(block) {
      # Define start index for each block
      start_idx <- (block - 1) * validation_length + 1
      
      # Determine end index based on block number
      end_idx <-
        if (block == num_blocks)
          nrow(data)
      else
        block * validation_length
      
      # Extract validation data based on indices
      validation_data <- data[start_idx:end_idx,]
      
      # The training data is the remaining data after excluding the validation data
      training_data <- data[-(start_idx:end_idx),]
      
      list(training = training_data, validation = validation_data)
    })
    
    return(data_partitions)
  }


#' Cross-Validate Partition for SVR Model
#'
#' This function performs cross-validation on the given data partitions using a
#' Support Vector Regression (SVR) model.
#' It returns the average correlation and mean squared error over the partitions.
#'
#' @param cost Numeric. The cost parameter for the SVR model.
#' @param nu Numeric. The nu parameter for the SVR model.
#' @param gamma (Optional) Numeric. The gamma parameter for the SVR model. Defaults to NULL.
#' @param data_partitions List of dataframes. The data partitions to use for training and validation.
#' @param bcv_folds (Optional) Numeric. Number of cross-validation folds. Defaults to 5.
#' @param signal_norm_names Character vector. Normalized signal column names.
#' @param predictors_norm_names Character vector. Normalized predictor column names.
#' @param lagged_cols Character vector. Names of lagged columns.
#' @param col_lags Numeric vector. Number of lags for each column.
#' @param vsvr_response Character. Response column name for the SVR model.
#' @param vsvr_tolerance (Optional) Numeric. Tolerance parameter for the SVR model. Defaults to 1.
#' @param silent (Optional) A logical for run the function silently (without printouts). Defaults to FALSE.
#' @param training_list_name (Optional) Character. Name of the training data list element. Defaults to "training".
#' @param validation_list_name (Optional) Character. Name of the validation data list element. Defaults to "validation".
#'
#' @return A list containing the average correlation (`avg_cor`) and average mean squared error (`avg_error`).
#'
#' @details
#' This function depends on the following internal package functions:
#' - \code{\link{generate_time_series_data}}: Constructs a dataset for time-series modeling.
#' - \code{\link{vsvr_model}}: Fits a Nu Support Vector Regression (SVR) model.
#'
#' \strong{Validations}:
#' The function employs several auxiliary validation functions to ensure inputs adhere to expected criteria:
#' - \code{\link{validate_pso_svr_params}}: Validates the parameters for the PSO and SVR.
#' - \code{\link{validate_character_vector_list}}: Ensures provided vectors are non-empty character vectors.
#' - \code{\link{validate_logical}}: Checks if an input is a logical value.
#' - \code{\link{validate_data_partitions}}: Validates the data partitions for correct structure and content.
#'
#' It's essential to provide inputs meeting these validation criteria to ensure the function's correct operation.
#'
#' @examples
#'
#' data_partition_sample <- list(list(training = data.frame(feature1 = rnorm(20),
#' feature2 = rnorm(20), feature1_1 = rnorm(20)), validation = data.frame(
#' feature1 = rnorm(20), feature2 = rnorm(20), feature1_1 = rnorm(20))))
#' cross_validate_partition(cost = 1, nu = 0.5, gamma = NULL, data_partitions = data_partition_sample,
#' bcv_folds = 1, signal_norm_names = c("feature1", "feature2"),
#' predictors_norm_names = c("feature1"), lagged_cols = c("feature1"), col_lags = c(1),
#' vsvr_response = "feature2", vsvr_tolerance = 1)
#'
#' @importFrom stats sd cor
#' @importFrom utils modifyList
#'
#' @export
#'
cross_validate_partition <-
  function(cost,
           nu,
           gamma = NULL,
           data_partitions,
           bcv_folds = 5,
           signal_norm_names,
           predictors_norm_names,
           lagged_cols,
           col_lags,
           vsvr_response,
           vsvr_tolerance = 1,
           silent = FALSE,
           training_list_name = "training",
           validation_list_name = "validation") {
    
    # Validate params
    validate_pso_svr_params(list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      lags = col_lags
    ))
    validate_character_vector_list(list(signal_norm_names, predictors_norm_names, lagged_cols))
    validate_logical(silent, "silent")
    validate_data_partitions(data_partitions = data_partitions, blocks = bcv_folds)
    
    if (!is.numeric(vsvr_tolerance) || length(vsvr_tolerance) != 1)
      stop("The 'vsvr_tolerance' argument must be a single integer.")
    
    cors <- numeric(bcv_folds)
    errors <- numeric(bcv_folds)
    
    na_count <- 0
    
    # Common arguments for generate_time_series_data
    common_args <- list(
      data_cols = signal_norm_names,
      predictor_cols = predictors_norm_names,
      lagged_cols = lagged_cols,
      lag_values = col_lags,
      vsvr_response = vsvr_response
    )
    
    for (df_list in seq_len(bcv_folds)) {
      # Prepare data for validation
      args_validation <-
        utils::modifyList(list(input_df = data_partitions[[df_list]][[validation_list_name]],
                               is_training = FALSE),
                          common_args)
      new_data_validation <-
        do.call(generate_time_series_data, args_validation)
      
      # Prepare data for training
      args_training <-
        utils::modifyList(list(input_df = data_partitions[[df_list]][[training_list_name]],
                               is_training = TRUE),
                          common_args)
      data_partitions_training <-
        do.call(generate_time_series_data, args_training)
      
      # if (df_list == 1) {
      #   print("Cross validation train and test data: ")
      #   print(head(data_partitions_training, 6))
      #   print(head(new_data_validation, 6))
      # }
      #
      
      #blocked_cv_svr_time <- Sys.time()
      
      # Train SVR model
      svr_model <-
        vsvr_model(
          data = data_partitions_training,
          response_var = vsvr_response,
          cost = cost,
          nu = nu,
          gamma = gamma,
          tolerance = vsvr_tolerance
        )
      
      #print(Sys.time() - blocked_cv_svr_time)
      
      # Make predictions
      predictions <- predict(svr_model, new_data_validation)
      
      if (stats::sd(predictions) == 0) {
        if (!silent) {
          message(
            "STANDARD DEVIATION OF 'PREDICTIONS' IS ZERO: SVM TOLERANCE IS TOO HIGH FOR NUMBER OF LAGS USED"
          )
        }
        cors[df_list] <- NA
        errors[df_list] <- NA
        na_count <- na_count + 1
        next
      }
      
      target_vals <-
        data_partitions[[df_list]][[validation_list_name]][[vsvr_response]]
      
      # Compute and save correlation and MSE
      cors[df_list] <- stats::cor(predictions, target_vals)
      errors[df_list] <-
        sqrt(mean((target_vals - predictions) ^ 2))
    }
    
    return(list(
      avg_cor = mean(cors, na.rm = TRUE),
      avg_error = mean(errors, na.rm = TRUE),
      na_count = na_count
    ))
  }


parallel_cross_validate_partition <-
  function(cost,
           nu,
           gamma = NULL,
           data_partitions,
           bcv_folds = 5,
           signal_norm_names,
           predictors_norm_names,
           lagged_cols,
           col_lags,
           vsvr_response,
           vsvr_tolerance = 1,
           silent = FALSE,
           training_list_name = "training",
           validation_list_name = "validation") {
    # Validate params
    validate_pso_svr_params(list(
      cost = cost,
      nu = nu,
      gamma = gamma,
      lags = col_lags
    ))
    validate_character_vector_list(list(signal_norm_names, predictors_norm_names, lagged_cols))
    validate_logical(silent, "silent")
    validate_data_partitions(data_partitions = data_partitions, blocks = bcv_folds)
    
    if (!is.numeric(vsvr_tolerance) || length(vsvr_tolerance) != 1)
      stop("The 'vsvr_tolerance' argument must be a single integer.")
    
    cors <- numeric(bcv_folds)
    errors <- numeric(bcv_folds)
    
    # Common arguments for generate_time_series_data
    common_args <- list(
      data_cols = signal_norm_names,
      predictor_cols = predictors_norm_names,
      lagged_cols = lagged_cols,
      lag_values = col_lags,
      vsvr_response = vsvr_response
    )
    
    blocked_cv_svr_time <- Sys.time()
    
    registerDoParallel(cores = detectCores() - 1)
    
    results <-
      foreach(
        df_list = seq_len(bcv_folds),
        .packages = c("e1071", "dplyr"),
        .combine = 'rbind'
      ) %dopar% {
        source("R/TimeSeries.R")
        source("R/VSVMRegression.R")
        
        # Prepare data for validation
        args_validation <-
          utils::modifyList(list(input_df = data_partitions[[df_list]][[validation_list_name]],
                                 is_training = FALSE),
                            common_args)
        new_data_validation <-
          do.call(generate_time_series_data, args_validation)
        
        # Prepare data for training
        args_training <-
          utils::modifyList(list(input_df = data_partitions[[df_list]][[training_list_name]],
                                 is_training = TRUE),
                            common_args)
        data_partitions_training <-
          do.call(generate_time_series_data, args_training)
        
        # Train SVR model
        svr_model <-
          vsvr_model(
            data = data_partitions_training,
            response_var = vsvr_response,
            cost = cost,
            nu = nu,
            gamma = gamma,
            tolerance = vsvr_tolerance
          )
        
        # Make predictions
        predictions <- predict(svr_model, new_data_validation)
        
        if (stats::sd(predictions) == 0)
          next
        
        target_vals <-
          data_partitions[[df_list]][[validation_list_name]][[vsvr_response]]
        
        # Compute and save correlation and MSE
        cors[df_list] <- stats::cor(predictions, target_vals)
        errors[df_list] <-
          sqrt(mean((target_vals - predictions) ^ 2))
        
        # Asegúrate de retornar los resultados que necesitas
        c(cors = cors[df_list], errors = errors[df_list])
      }
    
    stopImplicitCluster()
    
    print(Sys.time() - blocked_cv_svr_time)
    
    # Calcula el promedio de cada columna
    average_results <- colMeans(results)
    
    # El promedio de 'cors' estará en la primera posición y 'errors' en la segunda
    average_cors <- average_results["cors"]
    average_errors <- average_results["errors"]
    
    return(list(avg_cor = average_cors,
                avg_error = average_errors))
    
  }
