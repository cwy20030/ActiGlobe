test_that ("TZ2UTC converts dates to correct UTC offsets for America/New_York", {
    # Input dates: one in DST (October 24, 2017) and one after DST ends (November 20, 2017)
    x <- as.Date (c ("2017-10-24", "2017-11-20"))
    result <- TZ2UTC (DT = x, TZ = "America/New_York")

    # Expected offsets: UTC-04:00 during DST, UTC-05:00 after DST
    expected <- c ("UTC-04:00", "UTC-05:00")

    # Check type and length
    expect_type (result, "character")
    expect_length (result, length (expected))

    # Check values
    expect_equal (result, expected)
})
