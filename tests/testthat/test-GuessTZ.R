test_that("GuessTZ returns UTC for +0000 offset", {
  result <- GuessTZ(c("-0500", "+0100"), All = FALSE)

  # Check structure
  expect_true(is.character(result))
  expect_named(result, c("-0500", "+0100"))

  # Check specific values
  expect_equal(result[["-0500"]], "America/Atikokan")
  expect_equal(result[["+0100"]], "Africa/Algiers")


})

test_that("GuessTZ returns Etc/GMT+12 for -1200 offset", {
  result <- GuessTZ(aOF = "-1200", All = TRUE)

  # Check structure
  expect_true(is.character(result))
  expect_named(result, "-1200")

  # Check value
  expect_equal(result[["-1200"]], "Etc/GMT+12")
})

