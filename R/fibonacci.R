#' Calcuate the nth Fibonacci number
#' This function calculates the nth Fibonacci number using an optimal iterative
#' method. It has O(n) time complexity
#'
#' @param n An integer representing the position in the Fibonacci sequence.
#'          Must be a non-negative value.
#' @return The nth Fibonacci number
#' @examples
#' fib(0)
#' fib(100)
#' fib(10)
#' @export

fib <- function(n) {
  if (n < 0) {
    stop("The input must be a non-negative value")
  }
  x <- 0
  y <- 1

  for (i in 1:n) {
    temp <- x
    x <- y
    y <- temp + x
  }
  return(x)
}
