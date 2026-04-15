test_that ("CosinorM.KDE works on FlyEast actigraphy data", {
    # Summarize FlyEast dataset ----------------------------------------------
    BdfList <- BriefSum (
        data = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00",
        TZ = "America/New_York"
    )

    # Extract actigraphy data for a single day
    data <- BdfList$data
    data <- subset (data, data$Date == "2017-10-27")

    # Fit cosinor model
    fit <- CosinorM.KDE (
        time = data$Time,
        activity = data$Activity
    )

    # ---- Structure checks ----
    # Fit object should be a list with expected component
    expect_true (is.list (fit))
    expect_true ("coef.cosinor" %in% names (fit))

    # ---- Relationship checks ----
    # Coefficients should be numeric and non-empty
    expect_type (fit$coef.cosinor, "double")
    expect_gt (length (fit$coef.cosinor), 0)

    # ---- Content checks ----
    # Compare coefficients against expected values with tolerance
    expected <- c (
        MESOR        = 177.327801,
        Amplitude.24 = 157.744980,
        Acrophase.24 = -2.895821,
        Beta.24      = -153.004740,
        Gamma.24     = -38.380048
    )
    expect_equal (fit$coef.cosinor, expected, tolerance = 1e-6)

    # ---- Error checks ----
    # Invalid inputs should trigger errors
    expect_error (tryCatch (
        CosinorM.KDE (
            time = data$Time,
            activity = data$Activity,
            bw = -1
        )
    ))
})
