% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{generate_time_series_data_helper}
\alias{generate_time_series_data_helper}
\title{Helper Function to Generate Time Series Data}
\usage{
generate_time_series_data_helper(lag_values, is_training, data_list)
}
\arguments{
\item{lag_values}{Numeric vector indicating the lags for each column in the
dataset.
This parameter specifies how many previous time steps are
used for prediction.}

\item{is_training}{Logical flag indicating whether the data is being prepared
for training purposes.
If TRUE, the data is used for model training; if FALSE,
it's used for validation or testing.}

\item{data_list}{A list containing various data configurations including
processed data, normalized signal and predictor names,
and the response variable name.}
}
\value{
The result from the generate_time_series_data function, typically a
 transformed and structured data frame suitable for time series analysis in
 model training or evaluation.
}
\description{
This function prepares parameters and calls the generate_time_series_data
function.
It organizes and structures data for model training and evaluation based on
the provided configurations. This includes setting up data columns, predictor
columns, lagged columns, and response variables for time series analysis.
}
\examples{
\dontrun{
  generate_time_series_data_helper(...)
}

}
