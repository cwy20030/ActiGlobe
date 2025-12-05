test_that("Prob.Inact runs on FlyEast data and returns logical vector", {
  # Load example dataset
  data(FlyEast)

  # Create quick summary of the recording with adjustment for daylight saving
 BdfList <-
       BriefSum (
           df = FlyEast,
           SR = 1 / 60,
           Start = "2017-10-24 13:45:00"
       )

   # Let's extract actigraphy data from a single day
   Bdf <- BdfList$Bdf

   df <- BdfList$df

   # Let's extract actigraphy data from a single day
   ## Extract Fourth day -------------
   fnDP <- sum (Bdf$nDataPoints [seq_len (3)])
   fDP <- fnDP + 1 ### Midnight of the second day
   eDP <- sum (Bdf$nDataPoints [ seq_len (4)])


   df <- df [fDP:eDP,]


   # Fit GLM and return logical vector
   inactive_flags <- Prob.Inact(y = df$Activity,
                                T = df$Time,
                                k = 12,
                                threshold = 3,
                                logical = TRUE)



   # Assertions
   expect_type(inactive_flags, "logical")
   expect_length(inactive_flags, nrow(df))
   expect_true(any(inactive_flags %in% c(TRUE, FALSE)))  # must contain logical values





   # Fit GLM and return summary table
   inactive_summary <- Prob.Inact(y = df$Activity,
                                T = df$Time,
                                k = 12,
                                threshold = 3,
                                logical = FALSE)


   # Expected data frame
   expected <- data.frame(
     start    = c(0.00000, 22.28333),
     duration = c(7.516667, 1.550000),
     end      = c(7.50000, 23.81667)
   )

   # Check equality (allowing for floating-point tolerance)
   expect_equal(inactive_summary, expected, tolerance = 1e-6)

})
