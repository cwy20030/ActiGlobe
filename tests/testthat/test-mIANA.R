test_that("mIANA returns a data frame with expected columns", {
  sIANA <- mIANA ()

  # Check type
  expect_s3_class (sIANA, "data.frame")

  # Check column names
  expect_named (sIANA, c ("Timezone_IANA", "TZ_Code", "Offset", "Standard_Offset"))

  # Check that it has rows
  expect_true (nrow(sIANA) > 0)

  # Spot check a known entry
  expect_true ("Europe/Amsterdam" %in% sIANA$Timezone_IANA)
  expect_true ("CEST" %in% sIANA$TZ_Code)
})


test_that("mIANA errors if Write = TRUE but Dir is NULL", {
  expect_error (mIANA (Write = TRUE, Dir = NULL),
               "A directory must be provided")
})
