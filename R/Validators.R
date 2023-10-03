#' Validate Main Inputs
#'
#' This function validates the main inputs for data processing and model building.
#'
#' @param data A dataframe with the data.
#' @param model A character string specifying the model. Valid values are "FIR", "NFIR", "ARX", and "NARX".
#' @param signal_names A character vector with signal names.
#' @param predictors_names A character vector with predictor names.
#' @param vsvr_response A single character string indicating the response variable for v-SVR.
#' @param excluded_cols (Optional) A character vector with column names to be excluded. Defaults to NULL.
#' @param multi (Optional) A logical indicating whether multi-response is used. Default is FALSE.
#' @param silent (Optional) A logical indicating if the function should run silently. Default is TRUE.
#'
#' @examples
#' data <- data.frame(signal = rnorm(100), predictor = rnorm(100))
#' PSOBrainModeler:::validate_inputs_main(data = data,
#'   model = "FIR",
#'   signal_names = c("signal"),
#'   predictors_names = c("predictor"),
#'   vsvr_response = "signal")
#'

validate_inputs_main <- function(data,
                                 model,
                                 signal_names,
                                 predictors_names,
                                 vsvr_response,
                                 excluded_cols = NULL,
                                 multi = FALSE,
                                 silent = FALSE) {
  validate_data(data)
  validate_model(model)
  validate_character_vector_list(list(signal_names, predictors_names, vsvr_response))
  
  # Validate vsvr_response
  if (length(vsvr_response) != 1) {
    stop("'vsvr_response' should be a single character value.")
  }
  
  # Validate excluded_cols
  if (!is.null(excluded_cols)) {
    validate_character_vector(excluded_cols, "excluded_cols")
    if (!all(excluded_cols %in% colnames(data))) {
      stop("Some columns in 'excluded_cols' are not present in the data.")
    }
  }
  
  validate_logical(multi, "multi")
  validate_logical(silent, "silent")
  
  invisible(NULL)
}

#' Validate Character Vector List
#'
#' Ensures that the provided input is a non-empty list with character vector elements.
#'
#' @param char_list A list. The list that should contain character vectors.
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' validate_character_vector_list(list(c("a", "b"), c("x", "y")))
#'
#' @export
validate_character_vector_list <- function(char_list) {
  if (is.null(char_list) ||
      length(char_list) == 0 || !is.list(char_list)) {
    stop("Input should be a non-empty list.")
  }
  for (i in seq_along(char_list)) {
    vec <- char_list[[i]]
    if (is.null(vec) || length(vec) == 0) {
      stop(sprintf("Element %d of the list is NULL or empty.", i))
    }
    if (!is.character(vec)) {
      stop(sprintf("Element %d of the list is not a character vector.", i))
    }
  }
}


#' Validate Character Vector
#'
#' Ensures that the provided vector is a non-empty character vector.
#'
#' @param vec A vector. The vector to validate.
#' @param name A character. A name or description for the vector, used in error messages.
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' validate_character_vector(c("a", "b"), "Example")
#'
#' @export
validate_character_vector <- function(vec, name) {
  if (is.null(vec) || length(vec) == 0 || !is.character(vec)) {
    stop(sprintf("'%s' is NULL, empty, or not a character vector.", name))
  }
}

#' Validate Logical Value
#'
#' Ensures that the provided value is a logical scalar.
#'
#' @param log_val Logical. The logical value to validate.
#' @param name Character. A name or description for the logical value, used in error messages.
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' validate_logical(TRUE, "Example")
#'
#' @export
validate_logical <- function(log_val, name) {
  if (!is.logical(log_val) || length(log_val) != 1) {
    stop(sprintf("'%s' should be a single logical value (TRUE or FALSE).", name))
  }
}

#' Validate Model Type
#'
#' Checks if the provided model type is one of the expected model types.
#'
#' @param model Character. The model type name.
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' validate_model("FIR")
#'
#' @export
validate_model <- function(model) {
  # Validate model
  valid_models <- c("FIR", "NFIR", "ARX", "NARX")
  if (!(model %in% valid_models)) {
    stop(sprintf(
      "Invalid model specified. Please select from %s.",
      paste(valid_models, collapse = ", ")
    ))
  }
}

#' Validate Data Frame
#'
#' Ensures that the provided data is a non-empty data frame.
#'
#' @param data A data frame. The data to validate.
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' validate_data(data.frame(a=1:5, b=6:10))
#'
#' @export
validate_data <- function(data) {
  # Validate data
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Data is either not a dataframe or is empty.")
  }
}

#' Validate Data Partitions
#'
#' Ensures that the provided data partitions are structured correctly.
#'
#' @param data_partitions List. List containing the data partitions.
#' @param blocks Numeric (optional). Expected number of blocks (partitions) in the data_partitions. Default is 5.
#' @param partitions_names Character vector (optional). Expected names for the elements inside each partition. Defaults to c("training", "validation").
#'
#' @return No return value, this function stops with a message if validation fails.
#'
#' @examples
#' data_partitions <- list(list(training=data.frame(a=1:3), validation=data.frame(a=4:5)))
#' validate_data_partitions(data_partitions, 1)
#'
#' @export
validate_data_partitions <-
  function(data_partitions,
           blocks = 5,
           partitions_names = c("training", "validation")) {
    # Check if data_partitions is a list
    if (!is.list(data_partitions)) {
      stop("The 'data_partitions' argument must be a list.")
    }
    
    if (!is.numeric(blocks) || length(blocks) != 1)
      stop("The argument 'blocks' must be a single integer.")
    
    # Check if 'blocks' is greater than or equal to 1
    if (blocks < 1)
      stop("The required number of elements, 'blocks', must be greater than or equal to 1.")
    
    # Check if the length of data_partitions equals blocks
    if (length(data_partitions) != blocks) {
      stop(sprintf(
        "'data_partitions' should have %d elements. Found %d.",
        blocks,
        length(data_partitions)
      ))
    }
    
    # Check each element of data_partitions
    for (i in seq_along(data_partitions)) {
      partition <- data_partitions[[i]]
      
      # Check if the element is a list with two elements
      if (!is.list(partition) || length(partition) != 2) {
        stop(sprintf(
          "Element %d of 'data_partitions' should be a list with 2 elements.",
          i
        ))
      }
      
      # Check if the names of the elements inside the sublist are "training" and "validation"
      if (!all(sort(names(partition)) == partitions_names)) {
        stop(
          sprintf(
            "Element %d of 'data_partitions' should have names 'training' and 'validation'.",
            i
          )
        )
      }
    }
  }