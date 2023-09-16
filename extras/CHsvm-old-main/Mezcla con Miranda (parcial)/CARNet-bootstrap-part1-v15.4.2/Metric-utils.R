
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


# To check!!!!!!
get.NMSE <-function(sim, obs, ...) UseMethod("get.NMSE")

get.NMSE.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)
  
  sim <- sim - mean(sim, ...)
  obs <- obs - mean(obs, ...)
  nmse <- mean((sim - obs)^2, ...)
  
  return(nmse)
}


# To check!!!!!!
get.NMSEa <-function(sim, obs, ...) UseMethod("get.NMSEa")

get.NMSEa.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)
  
  sim <- sim - mean(sim, ...)
  obs <- obs - mean(obs, ...)
  K <- sum((obs * sim) / sim^2)
  nmsea <- sum((obs - K * sim)^2) / (length(obs) * var(obs))
  
  return(nmsea)
}


get.R.squared <-function(sim, obs, ...) UseMethod("get.R.squared")

get.R.squared.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)
  
  r <- cor(sim, obs, ...)      
  return(r^2)
  
}

get.R.squared.matrix <- function(sim, obs, ...)
{
  rs <- rep(NA, ncol(obs))       
  f <- function(i, x ,y) { rs[i] <- get.R.squared.default(x[, i], y[, i], ...) }
  rs <- sapply(1:ncol(obs), f, x = sim, y = obs)            
  
  return(rs)
  
}

get.R.squared.data.frame <- function(sim, obs, ...)
{
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  get.R.squared.matrix(sim, obs, ...)
}




#
# Tests
#

.test1 <- function()
{
  A <- 1:10
  B <- c(11:14, NA, 16:20)
  C <- c(21:22, NA, 24:26, NA, 28:30)
  D <- c(31:39, NA)
  
  M1 <- matrix(c(A, B), ncol = 2)
  M2 <- matrix(c(C, D), ncol = 2)
  
  DF1 <- data.frame(A, B)
  DF2 <- data.frame(C, D)
  
  # 1
  print(get.MSE(A, C))
  print(get.MSE(A, C, na.rm = TRUE))
  cat("\n")
  
  # 2
  print(get.MSE(B, D))
  print(get.MSE(B, D, na.rm = TRUE))
  cat("\n")
  
  # 3
  print(get.MSE(M1, M2))
  print(get.MSE(M1, M2, na.rm = TRUE))
  cat("\n")
  
  # 4
  print(get.MSE(DF1, DF2))
  print(get.MSE(DF1, DF2, na.rm = TRUE))
  cat("\n")
}

.test2 <- function()
{
  A <- 1:10
  B <- c(11:14, NA, 16:20)
  C <- c(21:22, NA, 24:26, NA, 28:30)
  D <- c(31:39, NA)
  
  M1 <- matrix(c(A, B), ncol = 2)
  M2 <- matrix(c(C, D), ncol = 2)
  
  DF1 <- data.frame(A, B)
  DF2 <- data.frame(C, D)
  
  # 1
  print(get.R.squared(A, C))
  print(get.R.squared(A, C, use = "pairwise.complete.obs"))
  cat("\n")
  
  # 2
  print(get.R.squared(B, D, method = "spearman"))
  print(get.R.squared(B, D, method = "spearman", use = "pairwise.complete.obs"))
  cat("\n")
  
  # 3
  print(get.R.squared(M1, M2), method = "kendall")
  print(get.R.squared(M1, M2, method = "kendall", use = "pairwise.complete.obs"))
  cat("\n")
  
  # 4
  print(get.R.squared(DF1, DF2, method = "pearson"))
  print(get.R.squared(DF1, DF2, method = "pearson", use = "pairwise.complete.obs"))
  cat("\n")
}

.test3 <- function(n = 101, rho0 = 1.0)
{
  require(ggplot2)
  
  rho <- seq(from = -12.0, to = 12.0, length.out = n)
  rhohat <- rho / rho0
  
  s <- rep(1, n)
  i <- rhohat != 0
  s[i] <- sin(rhohat[i]) / rhohat[i]
  
  s2 <- 2 * s - 0.5
  
  df <- data.frame(x = c(rho, rho), y = c(s, s2),
                   signal = rep(c("cm", "2cm-0.5"), each = n))
  
  p <- ggplot(data = df, aes(x, y, colour = signal)) + geom_line()
  print(p)
  cat("Correlation (Pearson): ", cor(s, s2, method = "pearson"), "\n")
  cat("MSE: ", get.MSE(s, s2), "\n")
  cat("NMSE: ", get.NMSE(s, s2), "\n")
  cat("NMSEa: ", get.NMSEa(s, s2), "\n")
  
  df
}
