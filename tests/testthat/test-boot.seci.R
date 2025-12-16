test_that ("boot.seci successfully run for OLS multicosinor and KDE", {
    BdfList <- BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

    # Extract actigraphy data from a single day
    df <- BdfList$df
    df <- subset (df, df$Date == "2017-10-27")


    # Test Multicomponent Cosinor Model (OLS)
    fit <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = c (12, 24),
        method = "OLS"
    )
    BCI <- boot.seci (
        object = fit,
        ci_level = 0.95,
        n = 100
    )


    # ---- Structure checks ----
    expect_s3_class (BCI, "data.frame")
    expect_equal (
        colnames (BCI),
        c ("Estimate", "Std Error", "t value", "2.5%", "97.5%")
    )


    # ---- Relationship checks ----
    # Verify expected row names correspond to cosinor components
    expected_rows <- c (
        "MESOR", "Amplitude.12", "Amplitude.24", "Acrophase.12",
        "Acrophase.24","Beta.12", "Beta.24", "Gamma.12", "Gamma.24",
        "MESOR.ph", "Bathyphase.ph.time", "Trough.ph", "Acrophase.ph.time",
        "Peak.ph", "Amplitude.ph"
    )
    expect_equal (rownames (BCI), expected_rows)


    # ---- Content checks ----
    # Ensure all values are numeric
    expect_true (all (vapply (BCI, is.numeric, logical (1))))



    # ---- Error checks ----
    # Invalid bootstrap parameters should trigger errors
    expect_error (tryCatch (boot.seci (object = fit,
                                       ci_level = 1.5, n = 100)))
    expect_error (tryCatch (boot.seci (object = fit,
                                       ci_level = 0.95, n = -10)))


    # Test Gaussian Kernel Density Estimation (KDE)
    fit2 <- CosinorM.KDE (
        time = df$Time,
        activity = df$Activity
    )
    BCI2 <- boot.seci (object = fit2, ci_level = 0.95, n = 100)


    # ---- Structure checks ----
    expect_s3_class (BCI2, "data.frame")
    expect_equal (
        colnames (BCI2),
        c ("Estimate", "Std Error", "t value", "2.5%", "97.5%")
    )

    # ---- Relationship checks ----
    expected_rows <- c (
        "MESOR", "Amplitude.24", "Acrophase.24", "Beta.24", "Gamma.24",
        "MESOR.ph", "Bathyphase.ph.time", "Trough.ph", "Acrophase.ph.time",
        "Peak.ph", "Amplitude.ph"
    )
    expect_equal (rownames (BCI2), expected_rows)


    # ---- Content checks ----
    expect_true (all (vapply (BCI2, is.numeric, logical (1))))


    # ---- Error checks ----
    # Invalid bootstrap parameters should trigger errors
    expect_error (tryCatch (boot.seci (object = fit2,
                                       ci_level = 1.5, n = 100)))
    expect_error (tryCatch (boot.seci (object = fit2,
                                       ci_level = 0.95, n = -10)))
})
