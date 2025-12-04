test_that("DST2GL detects DST transition correctly", {
  dates <- c("2021-03-13", "2021-03-14", "2021-03-15")
  res <- sapply(dates, DST2GL, TZ = "America/Toronto")

  expect_type(res, "double")
  expect_equal(res, c("2021-03-13" = 0,
                      "2021-03-14" = -1,
                      "2021-03-15" = 0))
})
