% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VSVMRegression.R
\name{vsvr_model}
\alias{vsvr_model}
\title{v-Support Vector Regression (v-SVR) Model}
\usage{
vsvr_model(
  data,
  response_var,
  cost,
  nu,
  gamma = NULL,
  tolerance,
  cache_size = 100
)
}
\arguments{
\item{data}{Dataframe containing predictor variables and the response
variable.}

\item{response_var}{Character string specifying the name of the response
variable in the data frame.}

\item{cost}{Numeric value representing the cost parameter of the SVR model,
controlling the trade-off between model complexity and error.}

\item{nu}{Numeric value representing the nu parameter of the SVR model,
controlling the number of support vectors.}

\item{gamma}{Numeric value (optional) representing the kernel parameter for
the SVR model. Required for radial basis function (RBF) kernels.}

\item{tolerance}{Numeric value representing the tolerance for the stopping
criterion of the SVR algorithm.}

\item{cache_size}{Numeric value indicating the size of the cache for the SVM
algorithm. Defaults to 100.}
}
\value{
A list containing two elements: `svm_model` - an SVR model object
 from `e1071::svm`, and `max_iterations_warnings` - a count of any warnings
 that occurred during the maximum iterations of the model fitting process.
}
\description{
Fits a Nu Support Vector Regression (SVR) model using the `svm` function from
the `e1071` package. This function is designed for regression tasks using
Nu-SVR, a variant of SVM that uses a parameter nu to control the number of
support vectors.
}
\examples{
# Example of fitting an SVR model with a radial kernel
# model <- vsvr_model(data = my_data,
#                     response_var = "target",
#                     cost = 1,
#                     nu = 0.3,
#                     gamma = 0.01,
#                     tolerance = 0.001,
#                     cache_size = 100)
}
