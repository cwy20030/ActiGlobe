test_that("Demand returns Option A when user selects 1", {


  if (grepl ("devel", R.version$status) && Sys.info () [["sysname"]] == "Linux") {
    skip ("Skip on ubuntu-latest (devel)")
  }


  # mock readline to always return "1"
  mockery::stub (Demand, "readline", function (prompt = "") "1")

  result <- Demand (c ("Option A", "Option B", "Other"), "option")

  # ---- Content checks ----
  # ---- Relationship checks ----
  expect_equal (result, "Option A")

})



test_that("Invalid input to Demand prompt returns error!", {

  if (grepl("devel", R.version$status) && Sys.info()[["sysname"]] == "Linux") {
    skip("Skip on ubuntu-latest (devel)")
  }

  # mock readline to first return "2" (Option B), then "1"
  responses <- c("no", "Uh non", "Ouais", as.character ( sample (seq (9), 1)))
  mock_readline <- function(prompt = "", i) {
    res <- responses[i]
    res
  }


  # ---- Error checks ----
  for (i in seq_along(responses)){
    mockery::stub(Demand, "readline", mock_readline)


    expect_error( tryCatch( Demand(c("Option A", "Option B", "Other"), "option") ) )


  }


})

