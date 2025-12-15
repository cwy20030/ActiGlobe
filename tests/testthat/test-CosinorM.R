test_that ("CosinorM works on FlyEast actigraphy data", {
    # Summarize FlyEast dataset ----------------------------------------------
    BdfList <- BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )

    # Extract actigraphy data for a single day
    df <- BdfList$df
    df <- subset (df, df$Date == "2017-10-27")


    # Fit OLS cosinor model ----------------------------------------------
    fit <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = 24
    )

    # ---- Structure checks ----
    expect_true (is.list (fit))
    expect_true ("coef.cosinor" %in% names (fit))

    # ---- Relationship checks ----
    # Coefficients should be numeric and non-empty
    expect_type (fit$coef.cosinor, "double")
    expect_gt (length (fit$coef.cosinor), 0)

    # ---- Content checks ----
    # Compare coefficients against expected values with tolerance
    expected <- c (
        MESOR = 177.297222,
        Amplitude.24 = 161.296243,
        Acrophase.24 = -2.896002,
        Beta.24 = -156.456357,
        Gamma.24 = -39.215893
    )
    expect_equal (fit$coef.cosinor, expected, tolerance = 1e-6)

    # ---- Error checks ----
    # Invalid tau or method should trigger errors
    expect_error (tryCatch (CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = -24
    )))
    expect_error (tryCatch (CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = 24,
        method = "INVALID"
    )))


    # Fit FGLS cosinor model ----------------------------------------------
    fit2 <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = 24,
        method = "FGLS"
    )

    # ---- Structure checks ----
    expect_true (is.list (fit2))
    expect_true ("coef.cosinor" %in% names (fit2))

    # ---- Relationship checks ----
    expect_type (fit2$coef.cosinor, "double")
    expect_gt (length (fit2$coef.cosinor), 0)

    # ---- Content checks ----
    expected2 <- c (
        MESOR = 181.169023,
        Amplitude.24 = 183.040124,
        Acrophase.24 = -2.409542,
        Beta.24 = -136.146165,
        Gamma.24 = -122.343405
    )
    expect_equal (fit2$coef.cosinor, expected2, tolerance = 1e-6)
})
