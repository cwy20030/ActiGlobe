# ColeKripke ------------------------------------------------------------------
test_that ("ColeKripke passes senitary check", {

  n <- 200L
  ys <- rep (0, n)
  yw <- rep (50000, n)

  result.s   <- suppressWarnings (
    ColeKripke (y = ys, Epc = 60, CutPoint = 1, Pad = "Zero")
  )

  result.s10 <- suppressWarnings (
    ColeKripke (y = ys, Epc = 10, CutPoint = 1, Pad = "Zero")
  )

  result.w   <- suppressWarnings (
    ColeKripke (y = yw, Epc = 60, CutPoint = 1, Pad = NA)
  )

  result.w30 <- suppressWarnings (
    ColeKripke (y = yw, Epc = 30, CutPoint = 1, Pad = NA)
  )

  # ---- Structure checks ----
  # Returns a character vector of the same length as input
  expect_type   (result.s, "character")
  expect_length (result.s, n)
  expect_true (all (result.w %in% c ("S", "W", NA)))


  # ---- Relationship checks ----
  expect_all_true (result.s == result.s10)
  expect_true (identical (result.w, result.w30))

  # Edge epochs should be NA; interior epochs should be classified
  expect_true  (any  (is.na (result.w)))
  interior <- result.w30 [5:(200 - 2)]
  expect_false (any (is.na (interior)))

  # ---- Content checks ----
  expect_true (all (result.s == "S"))


  # ---- Error checks ----
  # Unsupported Epc should throw
  expect_error (
    ColeKripke (y = y, Epc = 45, CutPoint = 1),
    regexp = "Cole-Kripke algorithm is only defined"
  )
  expect_error (
    ColeKripke (y = y, Epc = 120, CutPoint = 1),
    regexp = "Cole-Kripke algorithm is only defined"
  )
})



test_that ("ColeKripke CutPoint tunes classification sensitivity", {

  Act <- FlyEast$Activity
  ln  <- length (Act)

  resList <- setNames (lapply (c (0.5, 1, 5.0), function (cp) {
    suppressWarnings (
    ColeKripke (y = Act, Epc = 60, CutPoint = cp, Pad = "Zero")
    )
  }),
  c ("Strict", "Default", "Lenient"))



  Res.rf <- ColeKripke (y = Act, Epc = 60, CutPoint = 1, Pad = "Reflect")

  # ---- Structure checks ----
  # All outputs have the same length
  expect_all_equal (vapply (resList, length, numeric(1)),  ln)
  expect_length (resList, 3)
  expect_length (Res.rf, ln)

  # ---- Relationship checks ----
  # Higher CutPoint -> more epochs classified as sleep (monotonic)
  Rslt <- vapply (resList, function (x)
    sum (x  == "S", na.rm = TRUE), numeric (1))

  expect_lte (Rslt [[1]],  Rslt [[2]])
  expect_lte (Rslt [[2]],  Rslt [[3]])

  # Changing pad will not change the results in our case
  expect_true (identical (Res.rf, resList$Default))
})





# Oakley ----------------------------------------------------------------------
test_that ("Oakley passes senitary check", {

  n <- 200L
  ys <- rep (0, n)
  yw <- rep (500, n)

  result.s   <- suppressWarnings (
    Oakley (y = ys, Epc = 60, CutPoint = 40, Pad = "Zero")
  )

  result.s15 <- suppressWarnings (
    Oakley (y = ys, Epc = 15, CutPoint = 40, Pad = "Zero")
  )

  result.w   <- suppressWarnings (
    Oakley (y = yw, Epc = 60, CutPoint = 40, Pad = NA)
  )

  result.w30 <- suppressWarnings (
    Oakley (y = yw, Epc = 30, CutPoint = 40, Pad = NA)
  )



  # ---- Structure checks ----
  # Returns a character vector of the same length as input
  expect_type   (result.s, "character")
  expect_length (result.s, n)
  expect_true (all (result.w %in% c ("S", "W", NA)))


  # ---- Relationship checks ----
  expect_all_true (result.s == result.s15)
  expect_false (identical (result.w, result.w30))

  # Edge epochs should be NA; interior epochs should be classified
  expect_all_true (is.na (result.w [c (1, 2, 199, 200)]))
  expect_all_true (is.na (result.w30 [c (seq_len (4L), seq (200 - 3L, 200))]))


  # ---- Content checks ----
  expect_all_true (c (result.s15, result.s) == "S")
  expect_all_true (na.exclude (c (result.w, result.w30)) == "W")

  # ---- Error checks ----
  # Unsupported Epc should throw
  expect_error (
    Oakley (y = y, Epc = 45, CutPoint = 1)
  )
  expect_error (
    Oakley (y = y, Epc = 60, CutPoint = "invalid"),
    regexp = "must be numeric or the string 'automatic'"
  )
  # Vector-valued CutPoint
  expect_error (
    Oakley (y = y, Epc = 60, CutPoint = c (10, 20)),
    regexp = "must be a single numeric value"
  )
})

test_that ("ColeKripke CutPoint tunes classification sensitivity", {

  Act <- FlyEast$Activity
  ln  <- length (Act)

  resList <- setNames (lapply (c (10, 40, 2000), function (cp) {
    suppressWarnings (
      Oakley (y = Act, Epc = 60, CutPoint = cp, Pad = "Zero")
    )
  }),
  c ("Strict", "Default", "Lenient"))



  Res.zero <- Oakley (y = Act, Epc = 60, CutPoint = 40, Pad = "Zero")

  # ---- Structure checks ----
  # All outputs have the same length
  expect_all_equal (vapply (resList, length, numeric(1)),  ln)
  expect_length (resList, 3)
  expect_length (Res.zero, ln)

  # ---- Relationship checks ----
  # Higher CutPoint -> more epochs classified as sleep (monotonic)
  Rslt <- vapply (resList, function (x)
    sum (x  == "S", na.rm = TRUE), numeric (1))

  expect_lte (Rslt [[1]],  Rslt [[2]])
  expect_lte (Rslt [[2]],  Rslt [[3]])

  # Changing pad will not change the results in our case
  expect_true (identical (Res.zero, resList$Default))

})



# .oakley_auto_threshold ------------------------------------------------------

test_that (".oakley_auto_threshold correctly computes threshold from
           active data", {

  set.seed (42)
  y <- sample (100:5000, 200, replace = TRUE)

  result <- .oakley_auto_threshold (y = y, Epc = 60, Scaler = 0.88888)

  # ---- Structure checks ----
  # Returns a single positive numeric
  expect_type   (result, "double")
  expect_length (result, 1L)
  expect_gt     (result, 0)

  # ---- Relationship checks ----
  # Verify against manual calculation
  CutMobile <- as.integer (60 / 15)
  NMobile   <- sum (y >= CutMobile, na.rm = TRUE)
  TMobile   <- NMobile * (60 / 60)
  expected  <- (sum (y) / TMobile) * 0.88888
  expect_equal (result, expected)

  # ---- Error checks ----
  expect_error (
    .oakley_auto_threshold (y = y, Scaler = 0)
  )
})





# eCircaCP --------------------------------------------------------------------

test_that ("eCircaCP correctly validates CutPoint boundaries", {

  skip_if_not_installed ("CircaCP")

  # ---- Error checks ----
  expect_error (
    eCircaCP (data = data.frame (), Epc = 60, CutPoint = -0.1),
    regexp = "must be a numeric scalar between 0 and 1"
  )
  expect_error (
    eCircaCP (data = data.frame (), Epc = 60, CutPoint = 1.5),
    regexp = "must be a numeric scalar between 0 and 1"
  )
  expect_error (
    eCircaCP (data = data.frame (), Epc = 10, CutPoint = 0.2),
    regexp = "not defined for epoch length"
  )
  expect_error (
    eCircaCP (data = data.frame (), Epc = 120, CutPoint = 0.2),
    regexp = "not defined for epoch length"
  )
})



test_that ("eCircaCP correctly classifies multi-day recording", {
  skip_if_not_installed ("CircaCP")
  data <- data.frame (Time     = strftime (FlyEast_adj$DateTime, "%H:%M:%S"),
                      Activity = FlyEast_adj$Activity,
                      Date     = FlyEast_adj$Date,
                      Hour     = FlyEast_adj$Hour)
  result <- eCircaCP (data     = data,
                      Epc      = 60,
                      CutPoint = 0.2,
                      Dist     = "ZAG",
                      NDays    = 5L)
  # ---- Structure checks ----
  # Returns a character vector matching input length
  expect_type   (result, "character")
  expect_length (result, n)

  # Labels are "S", "W", or NA only
  unique_labs <- unique (result [!is.na (result)])

  # Strip trailing "_N" suffix used when NDays is reduced
  stripped <- sub ("_[0-9]+$", "", unique_labs)
  expect_true (all (stripped %in% c ("S", "W")))

  # ---- Relationship checks ----
  # At least some epochs should be labelled sleep and some wake
  expect_true (any (grepl ("^S", result, perl = TRUE), na.rm = TRUE))
  expect_true (any (grepl ("^W", result, perl = TRUE), na.rm = TRUE))
  # ---- Error checks ----
})
