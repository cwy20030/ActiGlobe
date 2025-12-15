test_that("UTCwDST correctly detects DST presence", {
  res <- UTCwDST(UTCs = c(1, 13.75))

  # ---- Structure checks ----
  # Result should be a logical vector  (or list) of same length as input
  expect_true(is.logical(res) || is.list(res))
  expect_length(res, 2)

  # ---- Relationship checks ----
  # Each UTC offset should map to a DST presence indicator
  expect_equal(names(res), c("1", "13.75"))

  # ---- Content checks ----
  # Specific values: UTC+1 should be in DST, UTC+5 should not
  expect_equal(all(res[["1"]]), TRUE)
  expect_equal(all(res[["13.75"]]), TRUE)
})

test_that("UTCwDST correctly detects DST presence", {
  res <- UTCwDST(UTCs = c("UTC+01:00", "UTC+08:00"))

  # ---- Structure checks ----
  # Result should be a logical vector  (or list) of same length as input
  expect_true(is.logical(res) || is.list(res))
  expect_length(res, 2)

  # ---- Relationship checks ----
  # Each UTC offset should map to a DST presence indicator
  expect_equal(names(res), c("UTC+01:00", "UTC+08:00"))

  # ---- Content checks ----
  # Specific values: UTC+1 should be in DST, UTC+5 should not
  expect_equal(all(res[["UTC+01:00"]]), TRUE)
  expect_equal(all(res[["UTC+08:00"]]), FALSE)
})
