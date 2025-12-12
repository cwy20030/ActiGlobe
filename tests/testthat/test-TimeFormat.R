test_that("TimeFormat returns HH:MM:SS strings", {
  Time <- c("2017/05/02 23:00:01", "1970/01/02 05:10:33", "2000/02/28 07:00:00")
  res <- tryCatch(TimeFormat(Time))

  # ---- Structure checks ----
  # Result should be a character vector
  expect_type(res, "character")

  # ---- Relationship checks ----
  # Output length should be 1 (format string returned)
  expect_length(res, 1)

  # ---- Content checks ----
  # Each element should match HH:MM:SS format string
  expect_equal(res, "%H:%M:%S")
})

test_that("TimeFormat returns empty strings when the string is a date only", {
  Time <- c("2017/05/02 23:00:01", "2000/02/28 07:00", "1970/01/02",
            "2022/11/28 08:35 PM")

  res <- tryCatch(TimeFormat(Time))
  res2 <- tryCatch(TimeFormat(Time, as.time = TRUE))

  # ---- Structure checks ----
  expect_type(res, "character")
  expect_type(res2, "character")

  # ---- Relationship checks ----
  # Output lengths should match expectations
  expect_length(res, 1)
  expect_length(res2, 4)

  # ---- Content checks ----
  expect_equal(res, "%H:%M:%S")
  expect_equal(res2, c("23:00:01", "07:00:00", "", "20:35:00"))
})
