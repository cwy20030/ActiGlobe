test_that("skip R2P", {

  data(FlyEast)

   # Create quick summary of the recording with adjustment for daylight saving.
   BdfList <-
       BriefSum (
           df = FlyEast,
           SR = 1 / 60,
           Start = "2017-10-24 13:45:00",
           TZ = "America/New_York")

   # Let's extract actigraphy data from a single day
   Bdf <- BdfList$Bdf

   data (TLog)

Out <-
   R2P (Bdf = Bdf,
        D = TLog$date_Start,
        U = TLog$UTC_Offset)


  #### Structure checks -----------
expect_s3_class (Out, "data.frame")
expect_equal (colnames (Out), c ("Date","Recording_Period","UTC","Hour_to_Adjust"))
expect_equal (nrow(Out), 35)

#### Column type checks --------------------
expect_type (Out$Recording_Period, "integer")
expect_type (Out$UTC, "character")
expect_type (Out$Hour_to_Adjust, "double")

#### Value checks ------------------
expect_equal (unique (Out$Recording_Period), seq_len (4))
expect_equal (unique (Out$Hour_to_Adjust), c (0,12,13))
})
