test_that("Act2Daily Test Completed", {
  # Summarize FlyEast dataset
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

  # Reduce the data size to only the first 3 days
  Bdf <- Bdf[1:3, ]
  df <- df[1:sum(Bdf$nDataPoints), ]

  dfList <- Act2Daily(
    df = df,
    Bdf = Bdf,
    VAct = "Activity",
    VTm = "Time",
    Incomplete = FALSE,
    Travel = FALSE
  )


  # ---- Structure checks ----
  # Verify that the returned object has the expected structure
  expect_true(is.list(dfList))
  expect_equal(names(dfList), c("Daily_df", "df"))

  # ---- Relationship checks ----
  # Daily_df should correspond to the subset of Bdf (3 days)
  Dates <- names(dfList$Daily_df)
  expect_equal(length(Dates), nrow(Bdf))

  # ---- Content checks ----
  # Ensure the Daily_df contains the expected date labels
  expect_equal(Dates, c("2017-10-24", "2017-10-25", "2017-10-26"))
})
