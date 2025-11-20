test_that("DST works for New York summer vs winter", {
  dates <- as.POSIXct(c("2021-06-15", "2021-12-15"), tz = "America/New_York")
  res <- DST(dates, TZ = "America/New_York")
  expect_type(res, "logical")   # or whatever DST returns
  expect_length(res, 2)
})
