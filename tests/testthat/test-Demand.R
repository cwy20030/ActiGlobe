test_that("Demand returns Option A when user selects 1", {

   # mock readline to always return "1"
  mockery::stub (Demand, "readline", function (prompt = "") "1")
  result <- Demand (c ("Option A", "Option B", "Other"), "option")
  expect_equal (result, "Option A")

})
