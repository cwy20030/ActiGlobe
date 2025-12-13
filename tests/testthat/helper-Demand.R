test_that("Demand returns proper response", {
  # mock readline to always return "1"
  mockery::stub(Demand, "readline", function(prompt = "") "1")

  result <- Demand(c("Option A", "Option B", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "Option A")


  # Second Test -----------------------------
  # mock readline to always return "2"
  mockery::stub(Demand, "readline", function(prompt = "") "2")

  result <- Demand(c("NOOOO", "Yes", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "Yes")
})


test_that("Demand handles 'Other' option with custom input", {
  # Mock readline to return "3" first (select "Other"), then a custom path
  call_count <- 0
  mockery::stub(Demand, "readline", function(prompt = "") {
    call_count <<- call_count + 1
    if (call_count == 1) return("3")
    return("/custom/path")
  })

  result <- Demand(c("Option A", "Option B", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "/custom/path")
})