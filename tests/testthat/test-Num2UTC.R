test_that ("Num2UTC formats numeric offsets correctly", {
    x <- c (9.5, -7)
    res <- Num2UTC (x)

    # ---- Structure checks ----
    # Result should be a character vector of same length as input
    expect_type (res, "character")
    expect_length (res, 2)

    # ---- Relationship checks ----
    # Each numeric offset should correspond to a properly formatted UTC string
    expect_equal (names (res), NULL) # no names expected
    expect_equal (length (res), length (x))

    # ---- Content checks ----
    # Exact values should match expected UTC offsets
    expect_equal (res, c ("UTC+09:30", "UTC-07:00"))

    # ---- Error checks ----
    # Non-numeric input should trigger an error
    expect_warning (expect_error (tryCatch (Num2UTC (c ("invalid")))))
    # NA values should trigger an error
    expect_error (tryCatch (Num2UTC (c (NA))))
    # Extremely large offsets should trigger an error
    expect_error (tryCatch (Num2UTC (c (100))))
})
