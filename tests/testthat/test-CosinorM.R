test_that ("CosinorM works on FlyEast actigraphy data", {
    # Summarize FlyEast dataset
    BdfList <- BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

    # Extract actigraphy data for a single day
    df <- BdfList$df
    df <- subset (df, df$Date == "2017-10-27")

    # Fit ols cosinor model
    fit <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = 24
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
        MESOR = 177.297222,
        Amplitude.24 = 161.296243,
        Acrophase.24 = -2.896002,
        Beta.24 = -156.456357,
        Gamma.24 = -39.215893
    )

    # Compare with tolerance for floating-point
    expect_equal (fit$coef.cosinor, expected, tolerance = 1e-6)


    # Fit fgls cosinor model
    fit2 <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = 24,
        method = "FGLS"
    )

    # Check that fit object has expected structure
    expect_true (is.list (fit2))
    expect_true ("coef.cosinor" %in% names (fit2))

    # Coefficients should be numeric
    expect_type (fit2$coef.cosinor, "double")

    # Sanity check: coefficients should have length > 0
    expect_gt (length (fit2$coef.cosinor), 0)

    # Expected coefficients
    expected2 <- c (
        MESOR = 181.169023,
        Amplitude.24 = 183.040124,
        Acrophase.24 = -2.409542,
        Beta.24 = -136.146165,
        Gamma.24 = -122.343405
    )


    # Compare with tolerance for floating-point
    expect_equal (fit2$coef.cosinor, expected2, tolerance = 1e-6)
})
