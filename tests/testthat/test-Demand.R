test_that("Demand returns 1 when user selects 1", {
  if (grepl("devel", R.version$status) && Sys.info()[["sysname"]] == "Linux") {
    skip("Skip on ubuntu-latest (devel)")
  }

  # mock readline to always return "1"
  mockery::stub(Demand, "readline", function(prompt = "") "1")

  result <- Demand(c("1", "2", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "1")
})
