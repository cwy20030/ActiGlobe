test_that ("Act2Daily Test Completed", {
    # Summarize FlyEast dataset
    BdfList <- BriefSum (
        data = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )

    # Extract actigraphy data for a single day
    data <- BdfList$data

    # Quick summary of the recording
    Bdf <- BdfList$Bdf

    # Reduce the data size to only the first 3 days
    Bdf <- Bdf [1:3, ]
    data <- data [1:sum (Bdf$nDataPoints), ]

    dfList <- Act2Daily (
        data = data,
        Bdf = Bdf,
        VAct = "Activity",
        VTm = "Time",
        Incomplete = FALSE,
        Travel = FALSE
    )


    # ---- Structure checks ----
    # Verify that the returned object has the expected structure
    expect_true (is.list (dfList))
    expect_equal (names (dfList), c ("Daily_df", "data"))

    # ---- Relationship checks ----
    # Daily_df should correspond to the subset of Bdf (3 days)
    Dates <- names (dfList$Daily_df)
    expect_equal (length (Dates), nrow (Bdf))

    # ---- Content checks ----
    # Ensure the Daily_df contains the expected date labels
    expect_equal (Dates, c ("2017-10-24", "2017-10-25", "2017-10-26"))
})

test_that ("Act2Daily handles Incomplete recordings when Incomplete = TRUE", {
    # Summarize FlyEast dataset
    BdfList <- BriefSum (
        data = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )
    data <- BdfList$data
    Bdf <- BdfList$Bdf

    # Take first day which is incomplete
    Bdf <- Bdf [1, ]
    data <- data [1:Bdf$nDataPoints [1], ]
    dfList <- Act2Daily (
        data = data,
        Bdf = Bdf,
        VAct = "Activity",
        VTm = "Time",
        Incomplete = TRUE, # Keep incomplete recordings
        Travel = FALSE
    )
    # ---- Structure checks ----
    expect_true (is.list (dfList))
    expect_equal (names (dfList), c ("Daily_df", "data"))

    # ---- Content checks ----
    # Should include the incomplete day
    expect_true (length (dfList$Daily_df) > 0)
})

test_that ("Act2Daily handles different TUnit parameters", {
    BdfList <- BriefSum (
        data = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )
    data <- BdfList$data
    Bdf <- BdfList$Bdf
    Bdf <- Bdf [1:2, ]
    data <- data [1:sum (Bdf$nDataPoints), ]
    # Test with different TUnit values
    for (unit in c ("day", "minute", "second")) {
        dfList <- Act2Daily (
            data = data,
            Bdf = Bdf,
            TUnit = unit,
            VAct = "Activity",
            VTm = "Time"
        )

        # ---- Structure checks ----
        expect_true (is.list (dfList))
        expect_equal (names (dfList), c ("Daily_df", "data"))
    }
})

test_that ("Act2Daily handles custom VAct and VTm column names", {
    BdfList <- BriefSum (
        data = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )
    data <- BdfList$data
    Bdf <- BdfList$Bdf
    Bdf <- Bdf [1:2, ]
    data <- data [1:sum (Bdf$nDataPoints), ]

    # Rename columns
    names (data) [1] <- "CustomTime"
    names (data) [2] <- "CustomActivity"
    dfList <- Act2Daily (
        data = data,
        Bdf = Bdf,
        VAct = "CustomActivity",
        VTm = "CustomTime"
    )
    # ---- Structure checks ----
    expect_true (is.list (dfList))
    expect_equal (names (dfList), c ("Daily_df", "data"))
})
