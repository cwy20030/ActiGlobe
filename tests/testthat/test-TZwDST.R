test_that ("TZwDST works properly", {
  # Find the DST transition time for following timezones
  res  <- TZwDST (TZ = "Africa/Cairo",     Year = 2010)
  res1 <- TZwDST (TZ = "Antarctica/Vostok",Year = 1994)
  res2 <- TZwDST (TZ = "Asia/Taipei")


  res3 <- TZwDST (TZ = "Pacific/Apia", Year = 2011, Simple = FALSE)
  res4 <- TZwDST (TZ = "Asia/Tehran",  Year = 2009, Simple = FALSE)
  res5 <- TZwDST (TZ = "America/Lima", Year = 1986, Simple = FALSE)

  # ---- Structure checks ----
  # Result should be a logical vector of length 2
  expect_type (res,  "logical")
  expect_type (res1, "logical")
  expect_type (res2, "logical")
  expect_type (res3, "list")
  expect_type (res4, "list")
  expect_type (res5, "list")

  expect_length (res,  1)
  expect_length (res1, 1)
  expect_length (res2, 1)
  expect_length (res3, 13)
  expect_length (res4, 13)
  expect_length (res5, 13)

  expect_all_true (names (res3) %in%
                     c ("TZ_NAME","UTC_DST","DST_Start_Date",
                        "DST_Start_Time","UTC_Standard","DST_End_Date",
                        "DST_End_Time","Original_UTC","Shift_Start_Date",
                        "Shift_Start_Time","New_UTC","New_Date",
                        "New_Time"))

  # ---- Relationship checks ----
  expect_true (res != res1)
  expect_true (res1 == res2)

  expect_true (nrow (res3) == 1)
  expect_true (nrow (res4) == 1)
  expect_true (nrow (res5) == 1)


  # ---- Content checks ----
  expect_true (res)
  expect_all_false (c (res1, res2))


  expect_all_true (as.character (res3) ==
  c ("Pacific/Apia","UTC-10:00","2011-09-24","03:00:00","UTC-11:00",
     "2011-04-02","04:00:00","UTC-10:00","2011-12-30","00:00:00",
     "UTC+14:00","2011-12-31","00:00:00"))

  expect_all_true (as.character (res4 [1:7]) ==
  c ("Asia/Tehran","UTC+04:30","2009-03-22","00:00:00",
     "UTC+03:30","2009-09-22","00:00:00"))
  expect_all_true (is.na (unlist (res4 [8:13]) ) )

  expect_all_true (as.character (res5 [1:7]) ==
  c ("America/Lima","UTC-04:00","1986-01-01","00:00:00",
     "UTC-05:00","1986-04-01","00:00:00"))
  expect_all_true (is.na (unlist (res5 [8:13]) ) )


  # ---- Error checks ----
  # Invalid TZ argument should trigger an error
  expect_error (tryCatch (
    WhenTransit (TZ = "InvalidDate", Year = 2026)
    ))
})
