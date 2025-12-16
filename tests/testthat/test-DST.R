test_that ("DST works for New York summer vs winter", {
  dates <- as.POSIXct (c ("2021-06-15", "2021-12-15"), tz = "America/New_York")
  res <- DST (dates, TZ = "America/New_York")

  # ---- Structure checks ----
  # Result should be a logical vector of length 2
  expect_type (res, "logical")
  expect_length (res, 2)

  # ---- Relationship checks ----
  # Summer date should be DST  (TRUE), winter date should not  (FALSE)
  expect_true (res [1])
  expect_false (res [2])

  # ---- Content checks ----
  # Ensure the vector contains both TRUE and FALSE values
  expect_equal (unique (res), c (TRUE, FALSE))

  # ---- Error checks ----
  # Invalid TZ argument should trigger an error
  expect_error (tryCatch (DST (dates, TZ = "Invalid/TZ")))
})


test_that ("DST shows no change for Taipei summer vs winter", {
  dates <- as.POSIXct (c ("2021-06-15", "2021-12-15"), tz = "Asia/Taipei")
  res <- DST (dates, TZ = "Asia/Taipei")

  # ---- Structure checks ----
  # Result should be a logical vector of length 2
  expect_type (res, "logical")
  expect_length (res, 2)

  # ---- Relationship checks ----
  # Both values should be identical  (no DST shift in Taipei)
  expect_true (all (res == res [1]))

  # ---- Content checks ----
  # Ensure the vector contains only one unique value
  expect_equal (length (unique (res)), 1)
})
