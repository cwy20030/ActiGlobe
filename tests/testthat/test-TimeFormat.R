test_that("TimeFormat returns HH:MM:SS strings", {
  Time <- c ("2017/05/02 23:00:01", "1970/01/02 05:10:33", "2000/02/28 07:00:00")

  res <- tryCatch (TimeFormat (Time))

  # Expect a character vector
  expect_type (res, "character")

  # Expect the same length as input
  expect_length (res, 1)

  # Each element should match HH:MM:SS format
  expect_equal (res == "%H:%M:%S")
})
