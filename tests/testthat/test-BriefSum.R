test_that("BriefSum returns expected structure and TZ_code values", {
  data(FlyEast)

  BdfList <- BriefSum(
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-19 13:45:00",
    TZ = "America/Toronto"
  )

  # Structure: should be a list with two elements
  expect_true(is.list(BdfList))
  expect_equal(names(BdfList), c("Bdf", "df"))

  # TZ_code should be a character vector containing both EDT and EST
  tz_codes <- unique(BdfList$Bdf$TZ_code)
  expect_true(all(c("EDT", "EST") %in% tz_codes))

  # Optional: check that TZ_code switches at least once
  expect_equal(length(unique(BdfList$Bdf$TZ_code)), 2)
})
