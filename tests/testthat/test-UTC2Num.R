test_that ("Num2UTC converts numeric offsets correctly", {
    x <- c (9.5, -7)
    result <- Num2UTC (x)

    # Check that result is character vector of expected length
    expect_type (result, "character")
    expect_length (result, length (x))

    # Example expected outputs (adjust to your function’s actual behavior)
    # Suppose 9.5 → "UTC+09:30", -7 → "UTC-07:00"
    expect_equal (result [1], "UTC+09:30")
    expect_equal (result [2], "UTC-07:00")
})
