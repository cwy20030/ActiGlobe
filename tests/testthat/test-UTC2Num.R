test_that("UTC2Num converts numeric offsets correctly", {
    x <- c ("UTC+09:30", "UTC-07:00")
    result <- UTC2Num(x)

    # ---- Structure checks ----
    # Result should be a character vector of same length as input
    expect_type(result, "double")

    # ---- Relationship checks ----
    # Each numeric offset should correspond to a properly formatted UTC string
    expect_equal(length(result), length(x))

    # ---- Content checks ----
    # Example expected outputs (adjust to your function’s actual behavior)
    expected <- c (
        "+09:30" = 9.5,
        "-07:00" = -7
    )
    expect_equal (result, expected)

    # ---- Error checks ----
    # Non-numeric input should trigger an error
    expect_warning(expect_error(tryCatch(Num2UTC(c("invalid")))))
    # NA values should trigger an error
    expect_error(tryCatch(Num2UTC(c(NA))))
    # Extremely large offsets should trigger an error
    expect_error(tryCatch(Num2UTC(c(100))))
})
