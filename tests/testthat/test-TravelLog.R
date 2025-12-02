test_that("TravelLog returns a data frame when Write = FALSE", {
  res <- TravelLog(Write = FALSE)

  # Check type
  expect_s3_class(res, "data.frame")

  # Check dimensions
  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 1)

  # Check column names
  expect_equal(
    names(res),
    c("ID", "UTC_Offset", "Country_with_Daylight_Saving", "date_Start", "date_End")
  )

  # Check values
  expect_equal(res$ID, "ExampleID")
  expect_equal(res$UTC_Offset, "+05:00")
  expect_equal(res$Country_with_Daylight_Saving, "TRUE")
  expect_equal(res$date_Start, as.character(Sys.Date()))
  expect_equal(res$date_End, as.character(Sys.Date() + 1))
})
