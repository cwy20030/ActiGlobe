# tests/testthat/test-UTCwDST.R
test_that("UTCwDST detects DST correctly for given offsets", {
  # Offsets to test
  offsets <- c(1, 8)
  result <- UTCwDST(offsets)

  # Output should be a named logical vector of same length
  expect_type(result, "logical")
  expect_length(result, length(offsets))
  expect_named(result, as.character(offsets))

  # Check expected DST behavior:
  # UTC+1 (Central European Time) observes DST
  expect_true(result["1"])

  # UTC+8 (China Standard Time) does not observe DST
  expect_false(result["8"])
})
