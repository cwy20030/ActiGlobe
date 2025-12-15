test_that("DateFormat returns correct format string when as.date = FALSE", {
  # Use any date input; the function should return the format string
  DT <- c("2017/05/02", "2000/02/28", "1970/01/02")
  fmt <- DateFormat(DT, as.date = FALSE)

  # ---- Structure checks ----
  # Output should be a character scalar
  expect_type(fmt, "character")
  expect_length(fmt, 1)

  # ---- Relationship checks ----
  # Format string should correspond to the input date representation
  expect_equal(fmt, "%Y/%m/%d")

  # ---- Content checks ----
  # Ensure the format string contains year, month, and day placeholders
  expect_true(all(c("%Y", "%m", "%d") %in% strsplit(fmt, split = "/")[[1]]))

  # ---- Error checks ----
  # Invalid input should trigger an error
  expect_error(tryCatch(DateFormat(DT, as.date = "not_logical")))
})
