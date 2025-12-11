test_that("UnitFactor returns correct denominator for valid time units", {
  expect_equal(UnitFactor("day"),    24 * 3600)
  expect_equal(UnitFactor("hour"),   3600)
  expect_equal(UnitFactor("minute"), 60)
  expect_equal(UnitFactor("second"), 1)
})

test_that("UnitFactor is case-insensitive", {
  expect_equal(UnitFactor("Day"),    24 * 3600)
  expect_equal(UnitFactor("HOUR"),   3600)
  expect_equal(UnitFactor("Minute"), 60)
  expect_equal(UnitFactor("SECOND"), 1)
})

test_that("UnitFactor returns NA for invalid units before Demand fallback", {
  # Demand() is interactive, so we only check the NA branch
  expect_true(is.na(UnitFactor("week")))
})
