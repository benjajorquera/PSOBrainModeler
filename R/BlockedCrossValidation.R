#' Generate Data Partitions for Blocked K-fold Cross-Validation
#'
#' @param data Dataframe. The dataset to partition. Defaults to NULL.
#' @param num_blocks (Optional) Integer. The number of blocks to divide the dataset into. Defaults to 5.
#' @param validation_size (Optional) Numeric (between 0 and 1). Proportion of data to be used
#'  for validation in each partition. Defaults to 0.2.
#'
#' @return A list of partitions, each containing a training set and a validation set.
#'
#' @examples
#' data <- data.frame(x = 1:100, y = 101:200)
#' partitions <- blocked_cv(data, num_blocks = 5, validation_size = 0.2)
#' @export
blocked_cv <-
  function(data = NULL,
           num_blocks = 5,
           validation_size = 0.2) {
    # Validation Checks
    if (!is.data.frame(data) || nrow(data) == 0)
      stop("'data' is empty or is not a data frame.")
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
      validation_data <- data[start_idx:end_idx, ]
      
      # The training data is the remaining data after excluding the validation data
      training_data <- data[-(start_idx:end_idx), ]
      
      list(training = training_data, validation = validation_data)
    })
    
    return(data_partitions)
  }


#' Cross-Validate Partition for SVR Model
#'
#' This function performs cross-validation on the given data partitions using a
#'  Support Vector Regression (SVR) model.
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
#'
#' @return A list containing the average correlation (`avg_cor`) and average mean squared error (`avg_error`).
#'
#' @details
#' This function depends on the following internal package functions:
#' - \code{\link{generate_time_series_data}}: Constructs a dataset for time-series modeling.
#' - \code{\link{vsvr_model}}: Fits a Nu Support Vector Regression (SVR) model.
#'
#' @examples
#'
#' data_partition_sample <- list(list(training = data.frame(feature1 = rnorm(20),
#' feature2 = rnorm(20), feature1_1 = rnorm(20)), validation = data.frame(
#' feature1 = rnorm(20), feature2 = rnorm(20), feature1_1 = rnorm(20))))
#' cross_validate_partition(cost = 1, nu = 0.5, gamma = NULL, data_partition_sample,
#' bcv_folds = 1, signal_norm_names = c("feature1", "feature2"),
#' predictors_norm_names = c("feature1"), lagged_cols = c("feature1"), col_lags = c(1),
#' vsvr_response = "feature2", vsvr_tolerance = 1)
#'
#' @importFrom stats sd cor
#' @importFrom utils modifyList
#' @export
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
           silent = FALSE) {
    if (!is.numeric(cost))
      stop("The 'cost' argument must be numeric.")
    if (!is.numeric(nu))
      stop("The 'nu' argument must be numeric.")
    if (!is.null(gamma) &&
        !is.numeric(gamma))
      stop("The 'gamma' argument must be numeric or NULL.")
    if (!is.list(data_partitions))
      stop("The 'data_partitions' argument must be a list.")
    if (!is.numeric(bcv_folds) || length(bcv_folds) != 1)
      stop("The 'bcv_folds' argument must be a single integer.")
    
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
    
    for (df_list in seq_len(bcv_folds)) {
      # Prepare data for validation
      args_validation <-
        utils::modifyList(list(input_df = data_partitions[[df_list]]$validation,
                               is_training = FALSE),
                          common_args)
      new_data_validation <-
        do.call(generate_time_series_data, args_validation)
      
      # Prepare data for training
      args_training <-
        utils::modifyList(list(input_df = data_partitions[[df_list]]$training,
                               is_training = TRUE),
                          common_args)
      data_partitions_training <-
        do.call(generate_time_series_data, args_training)
      
      # Train SVR model
      svr_model_pso <-
        vsvr_model(data_partitions_training,
                   vsvr_response,
                   cost,
                   nu,
                   gamma,
                   vsvr_tolerance)
      
      # Make predictions
      predictions_pso <- predict(svr_model_pso, new_data_validation)
      
      if (stats::sd(predictions_pso) == 0) {
        if (!silent) {
          message(
            "STANDARD DEVIATION OF 'PREDICTIONS PSO' IS ZERO: SVM TOLERANCE IS TOO HIGH FOR NUMBER OF LAGS USED"
          )
        }
        return(list(avg_cor = NA, avg_error = NA))
      }
      
      target_vals <-
        data_partitions[[df_list]]$validation[[vsvr_response]]
      
      # Compute and save correlation and MSE
      cors[df_list] <- stats::cor(predictions_pso, target_vals)
      errors[df_list] <-
        sqrt(mean((target_vals - predictions_pso) ^ 2))
    }
    
    return(list(
      avg_cor = mean(cors, na.rm = TRUE),
      avg_error = mean(errors, na.rm = TRUE)
    ))
  }
