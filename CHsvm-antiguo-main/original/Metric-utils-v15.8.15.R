


get.MSE <-function(sim, obs, ...) UseMethod("get.MSE")

get.MSE.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)
  
  mse <- mean((sim - obs)^2, ...)
  
  return(mse)
}

get.MSE.matrix <- function(sim, obs, ...)
{
  # Check that 'sim' and 'obs' have the same dimensions
  if(!all.equal(dim(sim), dim(obs)))
    stop(paste0("Invalid argument: dim(sim) != dim(obs) ",
         "(", "[", paste(dim(sim), collapse = " "), "]", " != ",
         "[", paste(dim(obs), collapse = " "), "]", ")"))
  
  mse <- colMeans((sim - obs)^2, ...)
  
  return(mse)
}

get.MSE.data.frame <- function(sim, obs, ...)
{
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  get.MSE.matrix(sim = sim, obs = obs, ...)
  
}
