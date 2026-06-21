test_that ("IANA has correct structure and contents", {
  # -------------------------------------------------------------------
  # Define the expected column names for the full IANA dataset
  # -------------------------------------------------------------------
  expected_cols_IANA <- c (
    "Country_Name","Country_Code",
    "Timezone_IANA","TimeZone_Identifiers",
    "Standard_Time_Zone_long_name","Standard_TZ_Code",
    "Standard_Offset","Standard_Offset_Numeric",
    "Observes_DST",
    "DST_Time_Zone_long_name","DST_TZ_Code",
    "DST_Offset","DST_Offset_Numeric"
  )

  # ---- Structure checks ----
  # Ensure IANA is a data.frame
  expect_s3_class (iIANA, "data.frame")

  # Ensure IANA has exactly the expected column names
  expect_equal (colnames (iIANA), expected_cols_IANA)

  # ---- Relationship checks ----
  # mIANA() should return a subset of IANA with overlapping values
  sIANA <- mIANA ()

  # ---- Content checks ----
  # Check that all values in sIANA are present in IANA for key columns
  expect_true (all (sIANA$Timezone_IANA %in% iIANA$TZ_IANA))
  expect_true (all (sIANA$TZ_Code %in% iIANA$TZ_Code))
  expect_true (all (sIANA$Standard_Offset %in% iIANA$Offset_Numeric))
  expect_true (all (sIANA$DST_Offset_Numeric %in% sIANA$Offset))
  expect_true (all (sIANA$Standard_Offset %in% iIANA$Standard_Offset))
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
