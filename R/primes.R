#' Sieve of Eratosthenes
#'
#' Return a vector of prime numbers less than the given number using the
#' sieve of Eratosthenes method
#'
#' @param n An integer specifying the upper limit (exclusive) for finding prime
#' numbers.
#' @returns A vector of prime numbers less than \code{n}.
#' @details
#' Throws an error if \code{n} is less than 2.
#' @examples
#' sieve(10)
#' sieve(20)
#' @export

sieve <- function(n) {
  if (n < 1) {
    stop("Input must be greater than 0")
  }
  sieve <- rep(TRUE, n)
  sieve[1] <- FALSE
  prime <- 2

  while (prime^2 <= n) {
    if (sieve[prime]) {
      # Ensure we only call seq.int when the range is valid
      sieve[seq.int(prime^2, n, prime)] <- FALSE
    }
    # Find the next prime number
    prime <- prime + 1
    while (prime <= n && !sieve[prime]) {
      prime <- prime + 1
    }
  }
  return(which(sieve))
}

#' Next Prime Number
#'
#' Returns the next prime from a given number \code{n}.
#'
#' @param n A non-negative integer specifying the starting place for the
#' next prime.
#' @returns An integer value for the nearest prime number after \code{n}.
#' @details
#' Returns an error if \code{n} is negative.
#'
#' @examples
#' next_prime(3)
#' next_prime(25)
#' @export

next_prime <- function(n) {
  if (n <= 0) {
    stop("Input must be non-negative")
  }
  p <- n + 1
  vals <- 2:n
  while (TRUE) {
    if (!any((p %% vals) == 0)) {
      return(p)
    }
    p <- p + 1
  }
}
