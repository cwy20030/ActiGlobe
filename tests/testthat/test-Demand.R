test_that("Demand returns 1 when user selects 1", {

  if (grepl("devel", R.version$status) && Sys.info()[["sysname"]] == "Linux") {
    skip("Skip on ubuntu-latest (devel)")
  }

  # mock readline to always return "1"
  mockery::stub(Demand, "readline", function(prompt = "") "1")

  result <- Demand(c("Option A", "Option B", "Other"), "option")

  # ---- Content checks ----
  expect_equal(result, "Option A")
})

