test_that("Prob.Inact runs on FlyEast data and returns logical vector", {

   # Create quick summary of the recording with adjustment for daylight saving
   BdfList <- BriefSum(
      df = FlyEast,
      SR = 1 / 60,
      Start = "2017-10-24 13:45:00",
    TZ = "America/New_York"
   )

   # Extract actigraphy data for a single day --------------------------------
   Bdf <- BdfList$Bdf
   df  <- BdfList$df

   ## Extract Fourth day
   fnDP <- sum(Bdf$nDataPoints[seq_len(3)])
   fDP  <- fnDP + 1   # Midnight of the second day
   eDP  <- sum(Bdf$nDataPoints[seq_len(4)])
   df   <- df[fDP:eDP, ]


   # Fit GLM and return logical vector
   inactive_flags <- Prob.Inact(
      y = df$Activity,
      T = df$Time,
      k = 12,
      threshold = 3,
      logical = TRUE
   )

   # ---- Structure checks ----
   expect_type(inactive_flags, "logical")
   expect_length(inactive_flags, nrow(df))

   # ---- Relationship checks ----
   # Must contain logical values TRUE/FALSE
   expect_true(any(inactive_flags %in% c(TRUE, FALSE)))

   # ---- Content checks ----
   # Fit GLM and return summary table
   inactive_summary <- Prob.Inact(
      y = df$Activity,
      T = df$Time,
      k = 12,
      threshold = 3,
      logical = FALSE
   )

   expected <- data.frame(
      start    = c(0.00000, 22.28333),
      duration = c(7.516667, 1.550000),
      end      = c(7.50000, 23.81667)
   )

   # Check equality (allowing for floating-point tolerance)
   expect_equal(inactive_summary[,1], expected[,1], tolerance = 1e-6)
   expect_equal(inactive_summary[,2], expected[,2], tolerance = 1e-6)
   expect_equal(inactive_summary[,3], expected[,3], tolerance = 1e-6)

   # ---- Error checks ----
   # 0 activity vector should trigger an error
   expect_error(tryCatch(Prob.Inact(y = 0, T = df$Time)))
})
