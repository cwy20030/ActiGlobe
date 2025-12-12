test_that("TZ2UTC converts dates to correct UTC offsets for America/New_York", {
  # Input dates: one in DST (October 24, 2017) and
  # one after DST ends (November 20, 2017)
  x <- as.Date(c("2017-10-24", "2017-11-20"))
  result <- TZ2UTC(DT = x, TZ = "America/New_York")

  # ---- Structure checks ----
  # Result should be a character vector of same length as input
  expect_type(result, "character")
  expect_length(result, length(x))

  # ---- Relationship checks ----
  # Each date should correspond to the correct UTC offset
  expect_equal(length(result), length(x))

  # ---- Content checks ----
  # Expected offsets: UTC-04:00 during DST, UTC-05:00 after DST
  expected <- c("UTC-04:00", "UTC-05:00")
  expect_equal(result, expected)

  # ---- Error checks ----
  # Invalid timezone should trigger an error
  expect_error(tryCatch(TZ2UTC(DT = x, TZ = "Invalid/TZ")))
})
