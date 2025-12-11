test_that("Demand returns 1 when user selects 1", {
  # mock readline to always return "1"
  mockery::stub(Demand, "readline", function(prompt = "") "1")

  result <- Demand(c("Option A", "Option B", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "Option A")
         
})


test_that("Demand returns Other when user selects 2", {
  
  # mock readline to always return "3"
  mockery::stub(Demand, "readline", function(prompt = "") "2")

  result <- Demand(c("NOOOO", "Yes", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "Yes")
})
