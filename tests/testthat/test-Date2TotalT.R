test_that("Date2TotalT returns correct seconds for one day", {
  # ---- Structure checks ----
  # Single date should return a numeric scalar
  res <- Date2TotalT(as.Date("2022-12-31"), "hour")
  expect_true(is.numeric(res))
  expect_equal(length(res), 1)


  # ---- Content checks ----
  # The day should correspond to 24 hours
  expect_equal(res, 24)
})


test_that("Date2TotalT handles unit names case-insensitively", {
  d <- as.Date("2022-12-31")

  # ---- Structure checks ----
  # All calls should return numeric scalar
  expect_true(is.numeric(Date2TotalT(d, "second")))
  expect_true(is.numeric(Date2TotalT(d, "SECOND")))
  expect_true(is.numeric(Date2TotalT(d, "SeCoNd")))

  # ---- Relationship checks ----
  # All variants should return the same value
  expect_equal(Date2TotalT(d, "second"), Date2TotalT(d, "SECOND"))
  expect_equal(Date2TotalT(d, "SECOND"), Date2TotalT(d, "SeCoNd"))

  # ---- Content checks ----
  # All should equal 86400 seconds
  expect_equal(Date2TotalT(d, "second"), 86400)
  expect_equal(Date2TotalT(d, "SECOND"), 86400)
  expect_equal(Date2TotalT(d, "SeCoNd"), 86400)
})
