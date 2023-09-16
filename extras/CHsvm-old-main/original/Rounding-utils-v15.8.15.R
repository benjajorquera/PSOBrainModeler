


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

