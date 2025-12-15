test_that ("BriefSum returns expected structure and TZ_code values", {
    BdfList <- BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-19 13:45:00",
        TZ = "America/New_York"
    )

    # ---- Structure checks ----
    # Should be a list with two elements
    expect_true (is.list (BdfList))
    expect_equal (names (BdfList), c ("Bdf", "df"))

    # ---- Relationship checks ----
    # TZ_code values should relate correctly to the time zone transitions
    tz_codes <- unique (BdfList$Bdf$TZ_code)
    expect_equal (length (unique (BdfList$Bdf$TZ_code)), 2)

    # ---- Content checks ----
    # TZ_code should contain both EDT and EST
    expect_true (all (c ("EDT", "EST") %in% tz_codes))

    # ---- Error checks ----
    # Invalid SR argument should trigger an error
    expect_error (tryCatch (
        BriefSum (
            df = FlyEast,
            SR = -1,
            Start = "2017-10-19 13:45:00",
            TZ = "America/New_York"
        )
    ))
})
