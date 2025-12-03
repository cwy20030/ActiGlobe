test_that("Num2UTC formats numeric offsets correctly", {
  x <- c(9.5, -7)
  res <- Num2UTC(x)

  # Expect a character vector of length 2
  expect_type(res, "character")
  expect_length(res, 2)

  # Check exact values
  expect_equal(res, c("UTC+09:30", "UTC-07:00"))
})
