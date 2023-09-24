#' Internal PSO Brain Modeler Environment
#'
#' An environment to store global variables related to the PSO Brain Modeler.
#'
.psoBrainModelerEnv <- new.env(parent = emptyenv())

#' Package Load Initialization
#'
#' Initializes the global variables stored in the .psoBrainModelerEnv environment
#' when the package is loaded.
#'
#' @param libname The name of the library.
#' @param pkgname The name of the package.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # .onLoad Data frame to store pressure related data.
  assign("pressure_df", data.frame(), envir = .psoBrainModelerEnv)
  
  # .onLoad Data frame for processed data.
  assign("processed_data", data.frame(), envir = .psoBrainModelerEnv)
  
  # .onLoad List to store data partitions.
  assign("data_partitions", list(), envir = .psoBrainModelerEnv)
  
  # .onLoad Character vector for normalized signal names.
  assign("NORM_SIGNAL_NAMES", c(""), envir = .psoBrainModelerEnv)
  
  # .onLoad Character vector for normalized predictor names.
  assign("NORM_PREDICTORS_NAMES", c(""), envir = .psoBrainModelerEnv)
  
  # .onLoad Character value for normalized VSVR response.
  assign("NORM_VSVR_RESPONSE", "", envir = .psoBrainModelerEnv)
  
  # .onLoad Numeric value for VSVR tolerance.
  assign("VSVR_TOL", 1, envir = .psoBrainModelerEnv)
}
