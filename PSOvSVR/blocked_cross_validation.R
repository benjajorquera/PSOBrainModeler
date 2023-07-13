blocked_cv <- function(data)
{
  # Especifica el número de bloques y el tamaño de la validación
  num_blocks <- 5
  validation_size <- 0.2
  
  # Calcula el tamaño de la validación
  validation_length <- round(nrow(data) * validation_size)
  
  # Crea una lista vacía para almacenar los conjuntos de entrenamiento y validación
  data_partitions <- vector("list", num_blocks)
  
  row_indices <- nrow(data)
  
  # Itera sobre los bloques y crea los conjuntos de entrenamiento y validación
  for (i in 1:num_blocks) {
    if (i == 1) {
      validation_data <- data[1:validation_length, ]
      training_data <- data[(validation_length + 1):row_indices, ]
    }
    else if (i == num_blocks) {
      training_data <- data[1:(((i - 1) * validation_length) + 1), ]
      validation_data <-
        data[(((i - 1) * validation_length) + 2):row_indices, ]
    }
    else {
      training_data <- data[1:((i - 1) * validation_length), ]
      validation_data <-
        data[(((i - 1) * validation_length) + 1):(validation_length * i), ]
      training_data <-
        rbind(training_data, data[((validation_length * i) + 1):row_indices, ])
    }
    
    # Almacena los conjuntos en la lista
    data_partitions[[i]] <-
      list(training = training_data, validation = validation_data)
  }
  
  return(data_partitions)
}