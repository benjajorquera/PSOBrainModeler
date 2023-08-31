#' Generate data partitions for blocked cross-validation.
#'
#' This function takes a dataset, the number of blocks to divide it into,
#' and the desired validation size as input, and returns a list of data
#' partitions for blocked cross-validation. Each partition contains a
#' training set and a validation set.
#'
#' @param data The dataset to partition.
#' @param num_blocks The number of blocks to divide the dataset into.
#' @param validation_size The relative size of the validation data (between 0 and 1).
#'
#' @return A list of partitions, where each partition is a list containing the
#'         training and validation sets.
#'
#' @examples
#' data <- data.frame(x = 1:100, y = 101:200)
#' partitions <- blocked_cv(data, num_blocks = 5, validation_size = 0.2)
#'
#' @importFrom round round
#' @export
blocked_cv <- function(data, num_blocks, validation_size) {
  validation_length <- round(nrow(data) * validation_size)
  data_partitions <- lapply(1:num_blocks, function(block) {
    if (block == num_blocks) {
      start_idx <- (block - 1) * validation_length + 1
      training_data <- data[-(start_idx:nrow(data)), ]
      validation_data <- data[start_idx:nrow(data), ]
    } else {
      start_idx <- (block - 1) * validation_length + 1
      end_idx <- block * validation_length
      validation_data <- data[start_idx:end_idx, ]
      training_data <- data[-(start_idx:end_idx), ]
    }
    
    list(training = training_data, validation = validation_data)
  })
  
  return(data_partitions)
}
