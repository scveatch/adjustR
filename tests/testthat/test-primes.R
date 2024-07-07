test_that("Sieve works 5", {
  expect_equal(sieve(5), c(2, 3, 5))
})

test_that("Sieve stops negative", {
  expect_error(sieve(-1), "Input must be greater than 0")
})

test_that("Next Prime works 3", {
  expect_equal(next_prime(3), 5)
})

test_that("Next Prime stops negative", {
  expect_error(next_prime(-1), "Input must be non-negative")
})

test_that("Next Prime stops 0", {
  expect_error(next_prime(0), "Input must be non-negative")
})
