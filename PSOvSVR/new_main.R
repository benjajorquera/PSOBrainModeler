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
SIGNAL_NAMES <- c("MABP", "CBFV.L")
SEED <- 123
DATA_FOLDER <- "/Data"

# DATA MANAGEMENT PARAMETERS
MAX_LAG_NUMBER <- 8
BCV_FOLDS <- 5
BCV_VALIDATION_SIZE <- 0.2
PRESSURE_SIGNAL_START <- 3
PRESSURE_SIGNAL_RESPONSE_SIZE <- 40
BUTTER_FILTER_ORDER <- 2
BUTTER_FILTER_FS <- 0.2
MODEL_EXCLUDED_COLUMNS <-
  c("Time", "MABP", "CBFV.L", "CBFV.R", "etCO2")

# V-SVR PARAMETERS
VSVR_TOLERANCE <- 1
VSVR_KERNEL <- "radial"
VSVR_RESPONSE <- "CBFV.L_norm"

# PSO CONTROL PARAMETERS
HYPER_PARAMS_LOWER_BOUNDS <- c(0.25, 0.1, (1 / (2 * 1024 ^ 2)), 1)
HYPER_PARAMS_UPPER_BOUNDS <-
  c(4096, 0.9, (1 / (2 * 0.0625 ^ 2)), MAX_LAG_NUMBER)
HYPER_PARAMS_INITIAL_VALUES <- c(NA, NA, NA, NA)
PSO_SWARM_SIZE <- 5
PSO_MAX_ITERATIONS <- 100
PSO_MAX_FUNCTION_CALLS <- 200
PSO_AVG_INFORMED_PARTICLES <- 0.5
PSO_GLOBAL_EXPLORATION_CONST <- 10
PSO_MAX_IT_WITHOUT_IMPROVEMENT <- 50
PSO_RESTART_TOLERANCE <- 0.5
PSO_HYBRID_TYPE <- "improved"
PSO_TYPE <- "SPSO2011"
PSO_VECTORIZATION <- TRUE
PSO_HYBRID_CONTROL <- list(maxit = 1)

# Set random seed
set.seed(SEED)

# Set the data directory and load data from patients
setwd(paste0(dirname(getActiveDocumentContext()$path), DATA_FOLDER))
data_file <- read.table(FILE_NAME, header = TRUE)

# Normalize the signals
data <- normalize_signals(data_file, SIGNAL_NAMES)

# Add time lags to the signals
data <- lag_normalized_signal(data, MAX_LAG_NUMBER, SIGNAL_NAMES)

# Perform k-folded blocked cross-validation
data_partitions <- blocked_cv(data, BCV_FOLDS, BCV_VALIDATION_SIZE)

# Generate a data frame for the smoothed negative pressure step response with a Butterworth filter
pressure_df <-
  add_pressure_step(
    PRESSURE_SIGNAL_START,
    PRESSURE_SIGNAL_RESPONSE_SIZE,
    BUTTER_FILTER_ORDER,
    BUTTER_FILTER_FS
  )

# Measure the time taken for optimization
time <- Sys.time()

# Perform parameter optimization using Particle Swarm Optimization (PSO)
resultados_pso <- psoptim(
  par = HYPER_PARAMS_INITIAL_VALUES,
  fn = pso_objective,
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

# Calculate the time taken for optimization
time <- Sys.time() - time

# Print the elapsed time and the optimized value
cat("Elapsed time for optimization:", time, "seconds\n")
cat("Optimized value:", resultados_pso$value, "\n")
