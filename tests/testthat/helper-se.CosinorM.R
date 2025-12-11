test_that("se.CosinorM works on FlyEast actigraphy data", {
    # Summarize FlyEast dataset ----------------------------------------------
    BdfList <- BriefSum(
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

    # Extract actigraphy data for a single day
    df <- BdfList$df
    df <- subset(df, df$Date == "2017-10-27")

    # Fit OLS cosinor model
    fit <- CosinorM(
        time = df$Time,
        activity = df$Activity,
        tau = 24
    )

    # Compute variance and Delta SEs
    res <- se.CosinorM(object = fit)

    # ---- Structure checks ----
    expect_true(is.list(res))
    expect_true(all(c("var", "se") %in% names(res)))

    # ---- Relationship checks ----
    # Variance and SE vectors should have same length and matching names
    expect_equal(names(res$var), gsub("^se\\.", "var.", names(res$se)))
    expect_equal(length(res$var), length(res$se))

    # ---- Content checks ----
    # Expected variance values
    expected_var <- c(
        var.MESOR = 6.113175e+01,
        var.Amplitude.24 = 1.272720e+02,
        var.Acrophase.24 = 5.924339e-03,
        var.Beta.24 = 1.431733e+02,
        var.Gamma.24 = 1.013537e+02
    )

    # Expected SE values
    expected_se <- c(
        se.MESOR = 7.81867924,
        se.Amplitude.24 = 11.28148916,
        se.Acrophase.24 = 0.07696973,
        se.Beta.24 = 11.96550255,
        se.Gamma.24 = 10.06745893
    )

    expect_equal(res$var, expected_var, tolerance = 1e-6)
    expect_equal(res$se, expected_se, tolerance = 1e-6)

    # ---- Error checks ----
    # Wrong type of object should trigger an error
    expect_error(
        tryCatch(se.CosinorM(object = fit, method = "unknown")))
})
