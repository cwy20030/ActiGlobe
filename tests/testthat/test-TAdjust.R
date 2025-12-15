test_that("TAdjust Test Completed", {
  # Summarize FlyEast dataset ----------------------------------------------
  BdfList <- BriefSum(
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-24 13:45:00",
    TZ = "America/New_York"
  )

  # Extract actigraphy data for a single day
  df <- BdfList$df

  # Quick summary of the recording
  Bdf <- BdfList$Bdf

  ## Reduce the data size to only the first 8 days
  Omit <- sum(Bdf$nDataPoints[1:7])
  Total <- sum(Bdf$nDataPoints[1:13])
  Bdf <- Bdf[8:13, ]

  # Create the new TLog ----------------------------------------------------
  tz_db <- data.frame(
    ID = c("Jane Doe", "Jane Doe"),
    UTC_Offset = c("UTC -04:00", "UTC +08:00"),
    Country_with_Daylight_Saving = c(TRUE, FALSE),
    date_Start = as.Date(c("2017/10/31", "2017/11/02")),
    date_End = as.Date(c(NA, NA))
  )

  Nadj <- TAdjust(
    Bdf = Bdf,
    TLog = tz_db,
    TZ = "America/New_York"
  )

  # ---- Structure checks ----
  expect_true(is.data.frame(Nadj))

  # ---- Relationship checks ----
  # Adjusted object should contain expected components
  expect_true(all(names(Bdf) %in% names(Nadj)))

  # ---- Content checks ----
  # Ensure adjusted Bdf has more or equal rows than the original
  expect_true(nrow(Nadj) >= nrow(Bdf))

  # ---- Error checks ----
  # Invalid TZ should trigger an error
  expect_error(tryCatch(TAdjust(Bdf = Bdf, TLog = tz_db, TZ = "Invalid/TZ")))
})
