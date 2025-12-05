test_that ("CosinorM.KDE works on FlyEast actigraphy data", {
    # Summarize FlyEast dataset
    BdfList <- BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )

    # Extract actigraphy data for a single day
    df <- BdfList$df
    df <- subset (df, df$Date == "2017-10-27")

    # Fit cosinor model
    fit <- CosinorM.KDE (
        time = df$Time,
        activity = df$Activity
    )

    # Check that fit object has expected structure
    expect_true (is.list (fit))
    expect_true ("coef.cosinor" %in% names (fit))

    # Coefficients should be numeric
    expect_type (fit$coef.cosinor, "double")

    # Sanity check: coefficients should have length > 0
    expect_gt (length (fit$coef.cosinor), 0)

    # Expected coefficients
    expected <- c (
        MESOR = 177.270466,
        Amplitude.24 = 157.852411,
        Acrophase.24 = -2.896046,
        Beta.24 = -153.117574,
        Gamma.24 = -38.371762
    )

    # Compare with tolerance for floating-point
    expect_equal (fit$coef.cosinor, expected, tolerance = 1e-6)
})
