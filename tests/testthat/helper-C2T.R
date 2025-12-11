test_that ("C2T discrete time conversion works", {
  times <- c ("01:00:00", "02:30:00", "03:15:00")

  res <- C2T (Time = times, Discrete = TRUE)

  # ---- Structure checks ----
  # Result should be a numeric vector of same length as input
  expect_true (is.numeric (res))
  expect_true (length (res) == 3)
  expect_equal (length (res), length (times))

  # ---- Relationship checks ----
  # Each converted time should correspond to hours + minutes/60
  expect_equal (res [1], 1)
  expect_equal (res [2], 2.5)
  expect_equal (res [3], 3.25)

  # ---- Content checks ----
  # Full vector should match expected values
  expect_equal (res, c (1, 2.5, 3.25))

  # ---- Error checks ----
  # Invalid time format should trigger an error
  expect_error (
    tryCatch (
      C2T (c ("invalid"),
           Discrete = TRUE)))
})



test_that ("C2T non-discrete time  time conversion works", {
  times <- c ("01:00:00", "02:30:00", "03:15:00")

  res <- C2T (Time = times, Discrete = FALSE)

  # ---- Structure checks ----
  # Result should be a numeric vector of same length as input
  expect_true (is.numeric (res))
  expect_true (length (res) == 3)
  expect_equal (length (res), length (times))

  # ---- Relationship checks ----
  # Each converted time should correspond to hours + minutes/60
  expect_equal (res [1], 0)
  expect_equal (res [2], 1.5)
  expect_equal (res [3], 2.25)

  # ---- Content checks ----
  # Full vector should match expected values
  expect_equal (res, c (0, 1.5, 2.25))


})


test_that ("C2T continuous time conversion works", {
  BdfList <- BriefSum (
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-24 13:45:00"
  )

  df <- BdfList$df
  Bdf <- BdfList$Bdf

  # Extract actigraphy data from the second day ------------------------------
  fnDP <- Bdf$nDataPoints[1]
  fDP <- fnDP + 1   # Midnight of the second day
  eDP <- sum (Bdf$nDataPoints[c (1, 2)])
  df <- df[fDP:eDP, ]

  res <- C2T (Time = df$Time, Discrete = TRUE)

  # ---- Structure checks ----
  # Result should be numeric and same length as the extracted data
  expect_true (is.numeric (res))
  expect_equal (length (res), nrow (df))

  # ---- Relationship checks ----
  # Sequence should correspond to minutes across a 24-hour day
  expected <- seq (0, 24, by = 1/60)
  expected <- expected[-length (expected)]  # remove last element to match length
  expect_equal (length (res), length (expected))

  # ---- Content checks ----
  # Ensure the conversion matches expected hourly sequence
  expect_equal (res, expected)

  # ---- Error checks ----
  # Passing non-DateTime values should trigger an error
  expect_error (
    tryCatch (
      C2T (Time = 12345,
           Discrete = TRUE)))
})
