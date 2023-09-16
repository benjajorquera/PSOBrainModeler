# Load required libraries
library(data.table)
library(mice)
library(signal)
library(dplyr)
library(e1071)
library(pso)
library(rstudioapi)

# Set the working directory and load custom functions
setwd(dirname(getActiveDocumentContext()$path))
source("utils.R")
source("blocked_cross_validation.R")
source("score_signal.R")
source("svr_model.R")
source("pso_objetive.R")

# GLOBAL VARIABLES
FILE_NAME <- "Sujeto1.txt"
SEED <- 123
DATA_FOLDER <- "/Data"

# DATA MANAGEMENT PARAMETERS
BCV_FOLDS <- 5
BCV_VALIDATION_SIZE <- 0.2
PRESSURE_SIGNAL_START <- 3
PRESSURE_SIGNAL_RESPONSE_SIZE <- 40
BUTTER_FILTER_ORDER <- 2
BUTTER_FILTER_FS <- 0.2
MAX_LAG_NUMBER <- 8

# V-SVR PARAMETERS
VSVR_TOLERANCE <- 3
VSVR_RESPONSE <- "CBFV.L_norm"

# PSO CONTROL PARAMETERS
PSO_SWARM_SIZE <-
  3 # Swarm size, if type == SPSO2011: defaults is 40, else defaults to 'floor(10+2*sqrt(length(par)))', with "par" params vector
# 5

PSO_MAX_ITERATIONS <- 3 # Defaults to '1000'
# 200

PSO_MAX_FUNCTION_CALLS <-
  5 # Maximum number of function evaluations, defaults to 'Inf'
# 500

PSO_AVG_INFORMED_PARTICLES <-
  0.5 # Average percentage of informants for each particle. A value of 1 implies that all particles are fully informed. Defaults to '1-(1-1/s)^k'

PSO_GLOBAL_EXPLORATION_CONST <-
  10 # Global exploration constant. Defaults to '5+log(2)'

PSO_MAX_IT_WITHOUT_IMPROVEMENT <-
  2 # Maximum number of iterations without improvement. Defaults to 'Inf'
# 100

PSO_RESTART_TOLERANCE <-
  0.5 # Tolerance for restarting: restarts if max( distance(best_particle, all_other_particles)) < reltol*d

PSO_HYBRID_TYPE <- "improved"
#' If true, each normal PSO position update is followed by an L-BFGS-B search with the provided position as initial guess.
#' This makes the implementation a hybrid approach. Defaults to FALSE which disables BFGS for the local search.
#' Note that no attempt is done to control the maximal number of function evaluations within the local search step (this can be done separately through hybrid.control)
#' but the number of function evaluations used by the local search method counts towards the limit provided by maxf AFTER the local search returns.
#' To support a broader class of hybrid approaches a character vector can also be supplied with “off” being equivalent to false,
#' “on” equivalent to true, and “improved” implying that the local search will only be performed when the swarm finds an improvement.

PSO_TYPE <- "SPSO2011"
#' Character vector which describes which reference implementation of SPSO is followed.
#' Can take the value of “SPSO2007” or “SPSO2011”. Defaults to “SPSO2007”.

PSO_VECTORIZATION <-
  TRUE # Logical; if TRUE the particles are processed in a vectorized manner, defaults to 'FALSE'

PSO_HYBRID_CONTROL <-
  list(maxit = 1) # List with any additional control parameters to pass on to optim when using L-BFGS-B for the local search. Defaults to NULL.

# Set random seed
set.seed(SEED)

# Set the data directory and load data from patients
setwd(paste0(dirname(getActiveDocumentContext()$path), DATA_FOLDER))
data_file <- read.table(FILE_NAME, header = TRUE)

# Generate a data frame for the smoothed negative pressure step response with a Butterworth filter
pressure_df <-
  add_pressure_step(
    PRESSURE_SIGNAL_START,
    PRESSURE_SIGNAL_RESPONSE_SIZE,
    BUTTER_FILTER_ORDER,
    BUTTER_FILTER_FS
  )



###############################################################################
# FIR

# Signals to be processed
SIGNAL_NAMES <- c("MABP", "CBFV.L")

EXCLUDED_COLS <- c("Time", "CBFV.R", "etCO2")

SIGNAL_NORM_NAMES <- c("MABP_norm", "CBFV.L_norm")

PREDICTORS_NORM_NAMES <- c("MABP_norm")

# Maximum and minimum number of signals lags, respectibly
MAX_LAG_NUMBERS <- c(8, 6, 6)
MIN_LAG_NUMBERS <- c(1, 1, 0)

HYPER_PARAMS_LOWER_BOUNDS <-
  c(0.25, 0.1, MIN_LAG_NUMBERS[1])
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096, 0.9, MAX_LAG_NUMBERS[1])
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA)

data <-
  process_dataframe(data_file,
                    EXCLUDED_COLS,
                    MAX_LAG_NUMBER,
                    SIGNAL_NAMES,
                    c("MABP"))

# Perform k-folded blocked cross-validation
data_partitions <- blocked_cv(data, BCV_FOLDS, BCV_VALIDATION_SIZE)

LAGGED_COLS <- c("MABP_norm")

#resultados_pso_FIR <- pso_optim(pso_objective_FIR)



################################################################################
# NFIR

HYPER_PARAMS_LOWER_BOUNDS <-
  c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), MIN_LAG_NUMBERS[1], MIN_LAG_NUMBERS[2])
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), MAX_LAG_NUMBERS[1], MAX_LAG_NUMBERS[2])
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA, NA)

#resultados_pso_NFIR <- pso_optim(pso_objective_NFIR)

################################################################################
# ARX

HYPER_PARAMS_LOWER_BOUNDS <-
  c(0.25, 0.1, MIN_LAG_NUMBERS[1], MIN_LAG_NUMBERS[2])
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096, 0.9, MAX_LAG_NUMBERS[1], MAX_LAG_NUMBERS[2])
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA)


LAGGED_COLS <- c("MABP_norm", "CBFV.L_norm")


data <-
  process_dataframe(data_file, EXCLUDED_COLS, MAX_LAG_NUMBER, SIGNAL_NAMES)

# Perform k-folded blocked cross-validation
data_partitions <- blocked_cv(data, BCV_FOLDS, BCV_VALIDATION_SIZE)

# Perform parameter optimization using Particle Swarm Optimization (PSO)
#resultados_pso_ARX <- pso_optim(linear_pso_objective)


###############################################################################


HYPER_PARAMS_LOWER_BOUNDS <-
  c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), MIN_LAG_NUMBERS[1], MIN_LAG_NUMBERS[2])
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), MAX_LAG_NUMBERS[1], MAX_LAG_NUMBERS[2])
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA, NA)

#resultados_pso_NARX <- pso_optim(pso_objective)


###############################################################################
# MULTI FIR

SIGNAL_NAMES <- c("MABP", "CBFV.L", "etCO2")

EXCLUDED_COLS <- c("Time", "CBFV.R")

SIGNAL_NORM_NAMES <- c("MABP_norm", "CBFV.L_norm", "etCO2_norm")

PREDICTORS_NORM_NAMES <- c("MABP_norm", "etCO2_norm")

HYPER_PARAMS_LOWER_BOUNDS <-
  c(0.25,
    0.1,
    MIN_LAG_NUMBERS[1],
    MIN_LAG_NUMBERS[3])
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096,
    0.9,
    MAX_LAG_NUMBERS[1],
    MAX_LAG_NUMBERS[3])
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA)

data <-
  process_dataframe(data_file,
                    EXCLUDED_COLS,
                    MAX_LAG_NUMBER,
                    SIGNAL_NAMES,
                    c("MABP", "etCO2"))

# Perform k-folded blocked cross-validation
data_partitions <- blocked_cv(data, BCV_FOLDS, BCV_VALIDATION_SIZE)

LAGGED_COLS <- c("MABP_norm", "etCO2_norm")

resultados_pso_FIR <- pso_optim(pso_objective_MULTI_FIR)


# Measure the time taken for optimization
#time <- Sys.time()
# Calculate the time taken for optimization
#time <- Sys.time() - time
# Print the elapsed time and the optimized value
#cat("Elapsed time for optimization:", time, "seconds\n")
#cat("Optimized value:", resultados_pso$value, "\n")

# v-SVR hyper params and signal lags to be optimized, need to have same size
# HYPER_PARAMS_LOWER_BOUNDS <-
#   c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), MIN_LAG_NUMBERS[1], MIN_LAG_NUMBERS[2])
# HYPER_PARAMS_UPPER_BOUNDS <-
#   c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), MAX_LAG_NUMBERS[1], MAX_LAG_NUMBERS[2])
# HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA, NA)
#
# if (length(HYPER_PARAMS_LOWER_BOUNDS) != length(HYPER_PARAMS_UPPER_BOUNDS) ||
#     length(HYPER_PARAMS_LOWER_BOUNDS) != length(HYPER_PARAMS_INITIAL_VALUES)) {
#   print("DIFFERENT SIZES OF PSO HYPERPARAMS CONTROL")
#   return()
# }
