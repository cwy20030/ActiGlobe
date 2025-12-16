test_that ("mIANA returns a data frame with expected columns", {
    sIANA <- mIANA ()

    # ---- Structure checks ----
    # Result should be a data frame with expected column names
    expect_s3_class (sIANA, "data.frame")
    expect_named (sIANA, c (
        "Timezone_IANA", "TZ_Code",
        "Offset", "Standard_Offset"
    ))

    # ---- Relationship checks ----
    # Data frame should have rows and valid mappings between columns
    expect_true (nrow (sIANA) > 0)
    expect_equal (length (unique (sIANA$Timezone_IANA)) > 0, TRUE)

    # ---- Content checks ----
    # Spot check known entries
    expect_true ("Europe/Amsterdam" %in% sIANA$Timezone_IANA)
    expect_true ("CEST" %in% sIANA$TZ_Code)
})


test_that ("mIANA errors if Write = TRUE but Dir is NULL", {
    # ---- Error checks ----
    # Invalid arguments should trigger errors
    expect_error (
        mIANA (Write = TRUE, Dir = NULL),
        "A directory must be provided"
    )
})
