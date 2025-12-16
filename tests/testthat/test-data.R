test_that ("IANA has correct structure and contents", {
  # -------------------------------------------------------------------
  # Define the expected column names for the full IANA dataset
  # -------------------------------------------------------------------
  expected_cols_IANA <- c (
    "Country_Name", "Country_Code",
    "Timezone_IANA", "TimeZone_Identifiers",
    "TZ_Code", "Offset",
    "Observes_DST", "Current_DST_Status",
    "Current_Abbreviation", "Current_Time_Zone_long_name", "Current_Offset",
    "Standard_Abbreviation", "Standard_Time_Zone_long_name", "Standard_Offset"
  )

  # ---- Structure checks ----
  # Ensure IANA is a data.frame
  expect_s3_class (IANA, "data.frame")

  # Ensure IANA has exactly the expected column names
  expect_equal (colnames (IANA), expected_cols_IANA)

  # ---- Relationship checks ----
  # mIANA() should return a subset of IANA with overlapping values
  sIANA <- mIANA ()

  # ---- Content checks ----
  # Check that all values in sIANA are present in IANA for key columns
  expect_true (all (sIANA$Timezone_IANA %in% IANA$Timezone_IANA))
  expect_true (all (sIANA$TZ_Code %in% IANA$TZ_Code))
  expect_true (all (sIANA$Offset %in% IANA$Offset))
  expect_true (all (sIANA$Standard_Offset %in% IANA$Standard_Offset))
})

test_that ("TLog has correct structure as compared to TravelLog template", {
  # -------------------------------------------------------------------
  # Generate a template TravelLog object for comparison
  # -------------------------------------------------------------------
  Ttemp <- TravelLog ()

  # ---- Structure checks ----
  # Ensure TLog has the same column names as the TravelLog template
  expect_equal (names (TLog), names (Ttemp))

  # ---- Content checks ----
  # Ensure the ID field in TLog is consistently "Jane Doe"
  expect_equal (unique (TLog$ID), "Jane Doe")
})
