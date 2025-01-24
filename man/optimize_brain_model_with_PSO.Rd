% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OptimizeBrainModelWithPSO.R
\name{optimize_brain_model_with_PSO}
\alias{optimize_brain_model_with_PSO}
\title{Optimize Brain Model Using PSO}
\usage{
optimize_brain_model_with_PSO(
  config,
  psoptim_config,
  data,
  model = "FIR",
  multi = FALSE,
  signal_names,
  excluded_cols = NULL,
  predictors_names,
  max_function_count = 1000,
  params_lower_bounds,
  params_upper_bounds,
  params_initial_values = NULL,
  vsvr_response,
  extra_col_name = NULL,
  silent = TRUE,
  plot_response = FALSE,
  initial_pressure_value = c(1),
  initial_response_value = 1,
  seed = 123,
  generate_response_predictions_cv = TRUE,
  basic_filter_check = TRUE,
  fn_count_threshold = 30,
  fitness_accuracy = 3,
  round_accuracy = 2,
  signif_accuracy = 3,
  show_progress_bar = FALSE,
  minimum_candidates = 10,
  fn_start_threshold = 100,
  time_on_fitness = FALSE,
  penalization_weight = 0.5
)
}
\arguments{
\item{config}{A list of configuration options for the brain modeler.}

\item{psoptim_config}{A list of configuration options for psoptim.}

\item{data}{A data frame containing the dataset to be optimized.}

\item{model}{Character string specifying the type of model to be optimized.
Should be one of "FIR", "NFIR", "ARX", or "NARX". Defaults to "FIR".}

\item{multi}{(Optional) Logical indicating if the model should be multivariate.
If TRUE, optimizes with two input variables; if FALSE, optimizes with one
input variable. Defaults to FALSE.}

\item{signal_names}{Vector of names representing signal columns.}

\item{excluded_cols}{(Optional) Vector of column names that should be excluded
from optimization. Defaults to NULL.}

\item{predictors_names}{Vector of predictor names for the model.}

\item{max_function_count}{(Optional) Maximum number of function evaluations.
Default is 1000.}

\item{params_lower_bounds}{Vector of lower bounds for model optimization
parameters.}

\item{params_upper_bounds}{Vector of upper bounds for model optimization
parameters.}

\item{params_initial_values}{(Optional) Vector of initial values for model
optimization parameters.
If provided, length should be equal for all parameter vectors. Defaults to
 NULL.}

\item{vsvr_response}{Name of the column representing the VSVR response.}

\item{extra_col_name}{(Optional) Additional column name to be included.
Defaults to NULL.}

\item{silent}{(Optional) A logical indicating if the function should run
silently. Defaults to TRUE.}

\item{plot_response}{(Optional) A logical to decide whether to plot the
response signal. Defaults to FALSE.}

\item{initial_pressure_value}{(Optional) Initial pressure value for the
optimization. Defaults to c(1).}

\item{initial_response_value}{Initial value set for the response variable in
the optimization process. Defaults to 1.}

\item{seed}{(Optional) Seed value for random number generation. Defaults to
123.}

\item{generate_response_predictions_cv}{Logical flag to determine if response
predictions should be generated. Defaults to TRUE.}

\item{basic_filter_check}{Logical flag indicating whether a basic filter
check is performed. Defaults to TRUE.}

\item{fn_count_threshold}{Integer threshold for a specific function count
condition, affecting the optimization flow. Defaults to 30.}

\item{fitness_accuracy}{Numeric value specifying fitness evaluation accuracy; default is 3.}

\item{round_accuracy}{Numeric value for rounding off the parameters.
Specifies the number of decimal places for rounding.}

\item{signif_accuracy}{Numeric value for significant figure accuracy.
Defines the number of significant digits to retain.}

\item{show_progress_bar}{Disables progress bar. Defaults to FALSE.}

\item{minimum_candidates}{Sets the lower limit for candidate consideration.}

\item{fn_start_threshold}{Determines the function's starting point if there are no candidates.}

\item{time_on_fitness}{Apply time to the objective function.}

\item{penalization_weight}{Numeric value for the weight in optimization penalization.}
}
\value{
Returns a list containing two elements:
  - `psoptim_result`: The result from the psoptim optimization process.
  - `pso_env`: The environment or context in which the psoptim optimization
      was performed.
}
\description{
This function implements blocked k-fold cross-validation for
 conducting hyperparameter optimization using Particle Swarm Optimization (PSO).
 It supports the creation of various model types, including FIR, NFIR, ARX,
 and NARX, which can be either univariate or multivariate. The function has
 been enhanced with the integration of a progress bar to provide visual
 feedback during the optimization process. Additionally, it utilizes an
 environment (env) for improved management and tracking of optimization
 parameters and results.
}
\details{
This function offers a comprehensive toolkit for the analysis and
modeling of biological signal data specific to individual patients.
It facilitates the training of SVR models to represent and predict
cerebral autoregulation phenomena.
}
\examples{

\dontrun{
 brain_modeler_config <- configure_pso_brain_modeler()
 psoptim_config <- configure_psoptim_control()

 optimize_brain_model_with_PSO(config = brain_modeler_config,
   psoptim_config = psoptim_config, ... )
}

}
