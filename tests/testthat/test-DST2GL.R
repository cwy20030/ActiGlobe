test_that ("DST2GL detects DST transition correctly", {
    dates <- c ("2021-03-13", "2021-03-14", "2021-03-15")
    res <- DST2GL (DT = dates, TZ = "America/New_York")

    # ---- Structure checks ----
    # Result should be a numeric vector with names corresponding to input dates
    expect_type (res, "double")
    expect_equal (names (res), dates)

    # ---- Relationship checks ----
    # ---- Content checks ----
    # Ensure the vector matches expected values for DST transition
    expect_equal (res, c (
        "2021-03-13" = 0,
        "2021-03-14" = -1,
        "2021-03-15" = 0
    ))

    # ---- Error checks ----
    # Invalid timezone should trigger an error
    expect_error (tryCatch (DST2GL (DT = dates, TZ = "Invalid/TZ")))
})
