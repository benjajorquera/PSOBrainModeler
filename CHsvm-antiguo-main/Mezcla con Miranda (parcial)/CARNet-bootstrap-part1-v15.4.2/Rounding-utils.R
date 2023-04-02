
is.wholenumber <- function(x, tol, ...) UseMethod("is.wholenumber")

is.wholenumber.default <- function(x, tol = .Machine$double.eps^0.5, ...)
{
  abs(x - round(x)) < tol
}


is.divisible <- function(x, y, tol, ...) UseMethod("is.divisible")

is.divisible.default <- function(x, y, tol = .Machine$double.eps^0.5, ...)
{
  is.wholenumber( x / y, tol, ...)
}


are.tolerably.equal <- function(x, y, tol, ...) UseMethod("tolerably.equal")

are.tolerably.equal <- function(x, y, tol = .Machine$double.eps^0.5, ...)
{
  abs(x - y) < tol
}


#
# Tests
#

.test1 <- function()
{
  x1 <- seq(0.1, 2.1, 0.1)
  x2 <- rep(c(0.1, 0.2, 0.3), each = 3, length.out = 21)
  x3 <- rev(x1)
  x4 <- rep(c(0.1, 0.2, 0.3), length.out = 21)
  
  df <- data.frame(x1, x2, x1 %/% x2, x1 %% x2, is.divisible(x1, x2))
  print(format(df, scientific = F))
  
  df <- data.frame(x3, x4, x3 %/% x4, x3 %% x4, is.divisible(x3, x4))
  print(format(df, scientific = F))
}

.test2 <- function()
{
  cat("sqrt(2) ^ 2 == 2", sqrt(2) ^ 2 == 2, "\n")
  cat("are.tolerably.equal(sqrt(2) ^ 2, 2)", are.tolerably.equal(sqrt(2) ^ 2, 2), "\n")
}
