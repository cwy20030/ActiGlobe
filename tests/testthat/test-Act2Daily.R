test_that ("Act2Daily Test Completed", {

  # Summarize FlyEast dataset
  BdfList <- BriefSum(
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-24 13:45:00",
    TZ = "America/Toronto"
  )

  # Extract actigraphy data for a single day
  df <- BdfList$df

  # Lets extract the quick summary of the recording
  Bdf <- BdfList$Bdf

  ## Reduce the data size to only the first 8 days.
  Bdf <- Bdf [1:3, ]


  df <- df[1:sum(Bdf$nDataPoints), ]

  dfList <-
    Act2Daily (
      df = df,
      Bdf = Bdf,
      VAct = "Activity",
      VTm = "Time",
      Incomplete = FALSE,
      Travel = FALSE
    )



  # Check that fit object has expected structure
  expect_true(is.list(dfList))
  expect_equal(names(dfList), c("Daily_df","df" ))


  # Daily_df should have
  Dates <- names(dfList$Daily_df)
  expect_equal(Dates, c("2017-10-24","2017-10-25","2017-10-26"))

})
