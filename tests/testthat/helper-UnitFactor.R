test_that ("UnitFactor returns correct denominator for valid time units", {
    # ---- Structure checks ----
    # Verify that the returned object has the expected structure
    expect_true (is.numeric (UnitFactor ("day")))
    expect_true (is.numeric (UnitFactor ("hour")))
    expect_true (is.numeric (UnitFactor ("minute")))
    expect_true (is.numeric (UnitFactor ("second")))

    # ---- Relationship checks ----
    # Ensure appropriate conversion
    expect_equal (UnitFactor ("day"), 24 * UnitFactor ("hour"))
    expect_equal (UnitFactor ("hour"), 60 * UnitFactor ("minute"))

    # ---- Content checks ----
    # Ensure the returned values are as expected
    expect_equal (UnitFactor ("day"), 24 * 3600)
    expect_equal (UnitFactor ("hour"), 3600)
    expect_equal (UnitFactor ("minute"), 60)
    expect_equal (UnitFactor ("second"), 1)


    # UnitFactor is case-insensitive -------------------------

    # ---- Content checks ----
    # Case-insensitive output
    expect_equal (UnitFactor ("Day"), 24 * 3600)
    expect_equal (UnitFactor ("HOUR"), 3600)
    expect_equal (UnitFactor ("Minute"), 60)
    expect_equal (UnitFactor ("SECOND"), 1)


    # ---- Error checks ----
    # Confirm that invalid units trigger an error
    expect_error (
        tryCatch (
            UnitFactor (
                x = "Day",
                method = "UnsupportedMethod"
            )
        )
    )
})


test_that ("UnitFactor handles invalid unit with Demand fallback", {
    # Mock readline to return "1" (select "day")
    mockery::stub (UnitFactor, "Demand", function (options, MESSAGE) "day")

    result <- UnitFactor ("invalid_unit", method = "Time")

    # ---- Structure checks ----
    expect_true (is.numeric (result))

    # ---- Content checks ----
    # Should return day value after Demand prompt
    expect_equal (result, 24 * 3600)
})
