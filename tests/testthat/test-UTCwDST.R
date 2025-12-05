test_that ("UTCwDST correctly detects DST presence", {
    res <- UTCwDST (UTC = c (1, 5))

    # Expect a logical vector of length 2
    expect_true (is.logical (res) || is.list (res))
    expect_length (res, 2)

    # Check specific values
    expect_equal (all (res [["1"]]), TRUE)
    expect_equal (all (res [["5"]]), FALSE)
})
