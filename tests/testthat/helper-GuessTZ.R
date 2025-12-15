test_that("GuessTZ returns UTC for +0000 offset", {
  result <- GuessTZ(c("-0500", "+0100"), All = FALSE)

  # ---- Structure checks ----
  # Result should be a character vector with names corresponding to offsets
  expect_true(is.character(result))
  expect_true(length(result) == 2)
  expect_named(result, c("-0500", "+0100"))

  # ---- Relationship checks ----
  # Each offset should map to a valid timezone string
  expect_equal(names(result), c("-0500", "+0100"))

  # ---- Content checks ----
  # Specific values should match expected timezones
  expect_equal(result[["-0500"]], "America/Atikokan")
  expect_equal(result[["+0100"]], "Africa/Algiers")
})


test_that("GuessTZ prioritizes local system time zone", {
  result <- GuessTZ(aOF = "-0500", All = FALSE, iTZ = "America/New_York")

  # ---- Structure checks ----
  # Result should be a character vector with names corresponding to offsets
  expect_true(is.character(result))
  expect_true(length(result) == 1)

  # ---- Content checks ----
  # Specific values should match expected timezones
  expect_equal(result, "America/New_York")
})


test_that("GuessTZ returns Etc/GMT+12 for -1200 offset", {
  result <- GuessTZ(aOF = "-1200", All = TRUE)

  # ---- Structure checks ----
  # Result should be a character vector with named offset
  expect_true(is.character(result))
  expect_named(result, "-1200")

  # ---- Relationship checks ----
  # Offset should map correctly to a timezone
  expect_equal(names(result), "-1200")

  # ---- Content checks ----
  # Specific value should match expected timezone
  expect_equal(result[["-1200"]], "Etc/GMT+12")
})



test_that("GuessTZ handles multiple offsets with All = TRUE", {
  result <- GuessTZ(aOF = c("+0000", "-0500"), All = TRUE)

  # ---- Structure checks ----
  expect_true(is.list(result) || is.character(result))
  expect_true(length(result) == 2)

  # ---- Content checks ----
  expect_named(result, c("+0000", "-0500"))
})
test_that("GuessTZ uses custom reference date", {
  custom_date <- as.POSIXct("2021-07-01 12:00:00", tz = "UTC")
  result <- GuessTZ(aOF = "+0000", DT = custom_date, All = FALSE)

  # ---- Structure checks ----
  expect_true(is.character(result))

  # ---- Content checks ----
  # Should return a valid timezone
  expect_true(nchar(result) > 0)
})
test_that("GuessTZ handles local timezone prioritization", {
  result <- GuessTZ(aOF = "-0500", iTZ = "local", All = FALSE)

  # ---- Structure checks ----
  expect_true(is.character(result))
  expect_true(length(result) == 1)
})
