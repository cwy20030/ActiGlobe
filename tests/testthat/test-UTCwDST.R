test_that("UTCwDST correctly detects DST presence", {
  res <- UTCwDST(UTCs = c(1, 8))

  # Expect a logical vector of length 2
  expect_type(res, "logical")
  expect_length(res, 2)

  # Check specific values
  expect_equal(res[["1"]], TRUE)
  expect_equal(res[["8"]], FALSE)
})
