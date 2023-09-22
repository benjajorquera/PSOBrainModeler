#' Generate Data Partitions for Blocked Cross-Validation
#'
#' This function takes a dataset, divides it into a specified number of blocks,
#' and creates partitions for cross-validation, ensuring that data points are
#' blocked together for validation. Each partition consists of a training set
#' and a validation set.
#'
#' @param data Dataframe. The dataset to partition.
#' @param num_blocks Integer. The number of blocks to divide the dataset into.
#' @param validation_size Numeric (between 0 and 1). Proportion of data to be used
#'        for validation in each partition.
#'
#' @return A list of partitions, each containing a training set and a validation set.
#'
#' @examples
#' data <- data.frame(x = 1:100, y = 101:200)
#' partitions <- blocked_cv(data, num_blocks = 5, validation_size = 0.2)
#' @export
blocked_cv <- function(data, num_blocks, validation_size) {
  # Validation Checks
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")
  if (!is.numeric(num_blocks) ||
      (num_blocks < 1) ||
      (num_blocks != round(num_blocks)))
    stop("'num_blocks' must be a positive integer.")
  if (!is.numeric(validation_size) ||
      validation_size <= 0 ||
      validation_size >= 1)
    stop("'validation_size' must be a number between 0 and 1.")
  
  validation_length <- round(nrow(data) * validation_size)
  
  if (validation_length * num_blocks > nrow(data)) {
    stop(
      "The combination of 'num_blocks' and 'validation_size' results in exceeding the data length."
    )
  }
  
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
