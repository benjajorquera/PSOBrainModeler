% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PSOHelpers.R
\name{extract_and_round_pso_params}
\alias{extract_and_round_pso_params}
\title{Extract and Round PSO Parameters}
\usage{
extract_and_round_pso_params(
  params,
  has_gamma = FALSE,
  n_lags = 1,
  round_accuracy = 2,
  signif_accuracy = 3
)
}
\arguments{
\item{params}{Numeric vector containing the parameters to be extracted.}

\item{has_gamma}{Logical indicating whether the gamma parameter is present.
Set to TRUE to include gamma in the extraction.}

\item{n_lags}{Integer specifying the number of lag parameters to extract.
Determines the count of lagged values included.}

\item{round_accuracy}{Numeric value for rounding off the parameters.
Specifies the number of decimal places for rounding.}

\item{signif_accuracy}{Numeric value for significant figure accuracy.
Defines the number of significant digits to retain.}
}
\value{
A list containing the extracted parameters:
  - `cost`: The cost parameter, rounded or significant based on its value.
  - `nu`: The nu parameter, rounded to 3 decimal places.
  - `lags`: A vector of lag parameters, each rounded to the nearest integer.
  If `has_gamma` is TRUE, the list also includes:
  - `gamma`: The gamma parameter, rounded or significant based on its value.
}
\description{
This function extracts parameters from a PSO optimization result, rounding
or applying significance as necessary.
It is designed to handle parameters including cost, nu, gamma (optional),
and lags.
}
\examples{
extract_and_round_pso_params(c(1.234, 2.345, 0.456, 3, 4, 5),
 has_gamma = TRUE, n_lags = 3)
extract_and_round_pso_params(c(0.234, 2.345, 3, 4), has_gamma = FALSE,
 n_lags = 2)

}
