test_that("boot.seci successfully run for OLS multicosinor and KDE", {


   BdfList <-
       BriefSum (
           df = FlyEast,
           SR = 1 / 60,
           Start = "2017-10-24 13:45:00"
       )

   # Let's extract actigraphy data from a single day
   df <- BdfList$df
   df <- subset (df, df$Date == "2017-10-27")


    # Test Multicomponent Cosinor Model ----------------
    fit <- CosinorM (
        time = df$Time,
        activity = df$Activity,
        tau = c (12, 24),
        method = "OLS"
    )

    # inspect coefficients
   BCI <-
    boot.seci (
        object = fit,
        level = 0.95,
        N = 100
    )
   # Structure checks----------------------------
   expect_s3_class(BCI, "data.frame")
   expect_equal(colnames(BCI), c("Estimate","Std Error","t value","2.5%","97.5%"))

   # Row names check ------------------------
   expected_rows <- c("MESOR","Amplitude.12","Amplitude.24","Acrophase.12","Acrophase.24",
                      "Beta.12","Beta.24","Gamma.12","Gamma.24","MESOR.ph",
                      "Bathyphase.ph.time","Trough.ph","Acrophase.ph.time",
                      "Peak.ph","Amplitude.ph")
   expect_equal(rownames(BCI), expected_rows)

   # Numeric check ----------------------
   expect_true(all(sapply(BCI, is.numeric)))







    # Test for Gaussian Kernel Density Estimation --------------------
    fit2 <- CosinorM.KDE (
        time = df$Time,
        activity = df$Activity
    )

    # inspect coefficients
  BCI2 <-
    boot.seci (
        object = fit2,
        level = 0.95,
        N = 100
    )

  ### Structure checks ------------------
  expect_s3_class(BCI2, "data.frame")
  expect_equal(colnames(BCI2), c("Estimate","Std Error","t value","2.5%","97.5%"))

  ### Row names check --------------------
  expected_rows <- c("MESOR","Amplitude.24","Acrophase.24","Beta.24","Gamma.24",
                     "MESOR.ph","Bathyphase.ph.time","Trough.ph","Acrophase.ph.time",
                     "Peak.ph","Amplitude.ph")
  expect_equal(rownames(BCI2), expected_rows)

  ### Numeric check -----------------------------
  expect_true(all(sapply(BCI2, is.numeric)))

})
