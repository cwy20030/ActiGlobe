test_that ("Rad2Hr converts radians to hours correctly", {
    # Single value: pi/2 radians should map to 6 hours when tau = 24
    expect_equal (Rad2Hr (pi / 2, tau = 24), 6)

    # Vector input: check multiple values
    input <- c (-pi / 2, 0, pi, 3 * pi / 2)
    expected <- c (18, 0, 12, 18)
    expect_equal (Rad2Hr (input, tau = 24), expected)
})

test_that ("Rad2Hr handles numeric vector output length", {
    input <- c (-pi / 2, 0, pi, 3 * pi / 2)
    result <- Rad2Hr (input, tau = 24)

    # Output should be same length as input
    expect_length (result, length (input))

    # Output should be numeric
    expect_type (result, "double")
})
