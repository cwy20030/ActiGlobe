test_that ("C2T discrete time conversion works", {
  times <- c ("01:00:00", "02:30:00", "03:15:00")

  res <- C2T (Time = times,
              Discrete = TRUE)


  expect_equal (res, c(1, 2.5, 3.25))

})

test_that ("C2T continuous time conversion works", {
  BdfList <-
    BriefSum (
      df = FlyEast,
      SR = 1 / 60,
      Start = "2017-10-24 13:45:00"
    )


  df <- BdfList$df
  Bdf <- BdfList$Bdf


  # Let's extract actigraphy data from a single day
  ## Extract Second day -------------
  fnDP <- Bdf$nDataPoints [1]

  fDP <- fnDP + 1 ### Midnight of the second day
  eDP <- sum (Bdf$nDataPoints [c (1,2)])


  df <- df [fDP:eDP,]


  res <- C2T (Time = df$DateTime,
              Discrete = TRUE)


  ### expected sequence --------------
  expected <- seq(0, 24, by = 1/60)  # from 0 to 0.1666... in steps of 0.0166...
  expected <- expected [-length (expected)]

  expect_equal (res, expected)


})
