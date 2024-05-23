test_that("Fibonacci works 0", {
  expect_equal(fib(0), 1)
})

test_that("Fibonacci works 10", {
  expect_equal(fib(10), 55)
})

test_that("Negative returns error", {
  expect_error(fib(-1), "The input must be a non-negative value")
})
