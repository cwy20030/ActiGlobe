# tests/testthat/test-DateFormat.R
test_that("DateFormat returns correct format string when as.date = FALSE", {
  # Use any date input; the function should return the format string
  DT <- c("2017/05/02", "2000/02/28", "1970/01/02")
  fmt <- DateFormat(DT, as.date = FALSE)

  # Check that the output is a character scalar
  expect_type(fmt, "character")
  expect_length(fmt, 1)

  # Check that the format string matches the expected value
  expect_equal(fmt, "%Y/%m/%d")
})

