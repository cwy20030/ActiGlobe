test_that("Date2TotalT returns correct seconds for one day", {
  # Single date should give 86400 seconds (24 hours)
  expect_equal(
    Date2TotalT(as.Date("2022-12-31"), "SeCoNd"),
    86400
  )
})

test_that("Date2TotalT handles unit names case-insensitively", {
  d <- as.Date("2022-12-31")
  expect_equal(Date2TotalT(d, "second"), 86400)
  expect_equal(Date2TotalT(d, "SECOND"), 86400)
  expect_equal(Date2TotalT(d, "SeCoNd"), 86400)
})
