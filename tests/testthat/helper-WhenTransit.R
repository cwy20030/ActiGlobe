# WhenTransit ---------------------------
test_that ("WhenTransit works properly", {
  # Find the DST transition time for following timezones
  res  <- WhenTransit (TZ = "America/New_York", Year = 2026)
  res1 <- WhenTransit (TZ = "Asia/Tehran",      Year = 2009)
  res2 <- WhenTransit (TZ = "Pacific/Apia",     Year = 2011)
  res3 <- WhenTransit (TZ = "Asia/Taipei")
  res4 <- WhenTransit (TZ = "Antarctica/Vostok",Year = 1994)
  res5 <- WhenTransit (TZ = "America/Managua",  Year = 1992)


  # ---- Structure checks ----
  # Result should be a logical vector of length 2
  expect_type (res,  "list")
  expect_type (res1, "list")
  expect_type (res2, "list")
  expect_type (res3, "list")
  expect_type (res4, "list")

  expect_length (res,  4)
  expect_length (res1, 4)
  expect_length (res2, 4)
  expect_length (res3, 4)
  expect_length (res4, 4)

  # ---- Relationship checks ----
  expect_true (nrow (res)  >= 4)
  expect_true (nrow (res1) >= 4)
  expect_true (nrow (res2) >= 4)
  expect_true (nrow (res3) >= 4)
  expect_true (nrow (res4) >= 4)
  expect_true (nrow (res5) >= 4)

  # ---- Content checks ----
  expect_true (all (is.na (res [res$Type %in% c ("TSFrom","TSTo"),
                                c ("Date", "Time", "UTC")] )))
  expect_true (all (res [res$Type == "DST", ] %in%
                      c ("DST","2026-03-08","02:00:00","UTC-04:00")))
  expect_true (all (res [res$Type == "ST", ] %in%
                      c ("ST","2026-11-01","02:00:00","UTC-05:00")))

  expect_true (all (is.na (res1 [res1$Type %in% c ("TSFrom","TSTo"),
                                 c ("Date", "Time", "UTC")] )))
  expect_true (all (res1 [res1$Type == "DST", ] %in%
                      c ("DST","2009-03-22","00:00:00","UTC+04:30")))
  expect_true (all (res1 [res1$Type == "ST", ] %in%
                      c ("ST","2009-09-22","00:00:00","UTC+03:30")))

  expect_true (all (res2 [res2$Type == "TSFrom", ] %in%
                      c ("TSFrom","2011-12-30","00:00:00","UTC-10:00")))
  expect_true (all (res2 [res2$Type == "TSTo", ] %in%
                      c ("TSTo","2011-12-31","00:00:00","UTC+14:00")))
  expect_true (all (res2 [res2$Type == "DST", ] %in%
                      c ("DST","2011-09-24","03:00:00","UTC-10:00")))
  expect_true (all (res2 [res2$Type == "ST", ] %in%
                      c ("ST","2011-04-02","04:00:00","UTC-11:00")))

  expect_true (all (is.na (res3 [c ("Date", "Time")] )))


  # ---- Error checks ----
  # Invalid TZ argument should trigger an error
  expect_error (tryCatch (
    WhenTransit (TZ = "InvalidDate", Year = 2026)
  ))
})





# .WhenTS ---------------------------
test_that (".WhenTS works properly", {
  # Find the DST transition time for following timezones
  res  <- .WhenTS(Date = "2026-03-08", TZ = "America/New_York")
  res1 <- .WhenTS(Date = "2026-11-01", TZ = "America/New_York")
  res2 <- .WhenTS(Date = "2010-09-26", TZ = "Pacific/Apia")
  res3 <- .WhenTS(Date = "2010-09-26", TZ = "Asia/Taipei")


  # ---- Structure checks ----
  # Result should be a character vector of length 4
  expect_type (res, "character")
  expect_type (res1, "character")
  expect_type (res2, "character")
  expect_type (res3, "character")

  expect_length (res, 4)
  expect_length (res1, 4)
  expect_length (res2, 4)
  expect_length (res3, 4)

  # ---- Relationship checks ----
  # All equal lengths
  expect_true  (length (res)  == length (res1) &&
                  length (res1) == length (res2) &&
                  length (res2) == length (res3) )


  # ---- Content checks ----
  expect_equal (res,
                c ("DST","2026-03-08","02:00:00","UTC-04:00"))
  expect_equal (res1,
                c ("ST","2026-11-01","02:00:00","UTC-05:00"))
  expect_equal (res2,
                c ("DST","2010-09-26","00:00:00","UTC-10:00"))
  expect_true (all (is.na (res3)))


  # ---- Error checks ----
  # Invalid TZ argument should trigger an error
  expect_error (tryCatch (.WhenTS (Date = "",
                                   TZ = "America/New_York")))
})







# .FormatTransitOutput ---------------------------
test_that (".FormatTransitOutput works properly", {

  mock_trans_dst <- matrix (
    data = c ("DST", "2011-09-24", "03:00:00", "UTC-10:00",
              "ST",  "2011-04-02", "04:00:00", "UTC-11:00"),
    nrow = 4
  )

  res <- .FormatTransitOutput (
    DJ.From  = "2011-12-30",
    DJ.To    = "2011-12-31",
    DJ.Time  = c ("00:00:00", "00:00:00"),
    DJ.UTC   = c ("UTC-10:00", "UTC+14:00"),
    TransDST = mock_trans_dst,
    Year     = 2011,
    TZ       = "Pacific/Apia",
    dUTC     = c (0, 0),
    UTCs     = rep (c (-10, -11, -10, 14),
                             c (92, 175, 96, 2))
  )

  # Setup an out-of-bounds matrix payload variant to test data cleanup filters
  mock_trans_oob <- matrix (
    data = c ("DST", "2026-03-08", "02:00:00", "UTC-04:00",
              "ST",  "2027-11-01", "02:00:00", "UTC-05:00"),
    nrow = 4
  )

  res_oob <- .FormatTransitOutput (
    DJ.From  = NA_character_,
    DJ.To    = NA_character_,
    DJ.Time  = rep (NA_character_, 2),
    DJ.UTC   = rep (NA_character_, 2),
    TransDST = mock_trans_oob,
    Year     = 2026,
    TZ       = "America/New_York",
    dUTC     = c (0, 0),
    UTCs     = rep (c (-5, -4, -5),
                    c (67, 238, 61)),
    Internal = FALSE
  )


  # ---- Structure checks ----
  expect_s3_class (res, "data.frame")
  expect_s3_class (res_oob, "data.frame")


  # ---- Relationship checks ----
  # Column dimensions must remain fixed even after
  # processing historical year deletions
  expect_true (ncol (res) == ncol (res_oob))


  # ---- Content checks ----
  # Confirm structural assembly of transition keys and
  # validation of out-of-bounds removal
  expect_true (all (c ("TSFrom", "TSTo", "DST", "ST") %in% res$Type))
  expect_false ("2027-11-01" %in% res_oob$Date)


  # ---- Error checks ----
  # Implicit tracking context handles data formatting;
  # errors cascade to root wrapper
})
