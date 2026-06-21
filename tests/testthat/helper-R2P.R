test_that ("skip R2P", {
    # Create quick summary of the recording with adjustment for daylight saving.
    BdfList <-
        BriefSum (
            data  = FlyEast,
            SR    = 1 / 60,
            Start = "2017-10-24 13:45:00",
            TZ    = "America/New_York"
        )

    # Let's extract actigraphy data from a single day
    Bdf <- BdfList$Bdf

    Out <- suppressWarnings (
        R2P (
            Bdf     = Bdf,
            Ds      = TLog$Date_Start,
            U       = TLog$UTC_Offset,
            has.DST = TLog$Country_with_Daylight_Saving
        )
        )

    #### Structure checks -----------
    expect_s3_class (Out, "data.frame")
    expect_equal (colnames (Out), c (
        "Date", "Prd",
        "UTC", "H2J",
        "hsDST"
    ))
    expect_equal (nrow (Out), 35)

    #### Column type checks --------------------
    expect_type (Out$Prd, "integer")
    expect_type (Out$UTC, "character")
    expect_type (Out$H2J, "double")

    #### Value checks ------------------
    expect_equal (unique (Out$Prd), seq_len (4))
    expect_equal (unique (Out$H2J), c (0, 12, 13))
})
