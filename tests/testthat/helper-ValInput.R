test_that("ValInput handles Act, Tm correctly", {

  # Act as activity ------------------------------------------
  # ---- Structure checks ----
  act <- ValInput(c("1", "2", "3"), type = "Act")
  expect_type(act, "double")
  expect_true(is.numeric(act))

  # ---- Relationship checks ----
  # ---- Content checks ----
  act <- ValInput(c("1", "2", "3"), type = "Act")
  expect_equal(unname(act), c(1, 2, 3))



  # ---- Error checks ----
  # Ensure Act rejects all-zero input
  expect_error(
    tryCatch(ValInput(c(0, 0, 0), type = "Act"))
  )

  # Act rejects NA/NaN/Inf
  expect_error(
    tryCatch(ValInput(c(1, NA, 3), type = "Act"))
  )
  expect_error(
    tryCatch(ValInput(c(1, Inf, 3), type = "Act"))
  )



  # Tm as time ------------------------------------------
  # ---- Structure checks ----
  # Mock C2T for testing
  C2T <- function(x, Discrete = TRUE) as.numeric(x)
  tm <- ValInput(c("1", "2", "3"), type = "Tm")
  expect_true(is.numeric(tm))

  # ---- Content checks ----
  # ---- Relationship checks ----
  tm <- ValInput(c("1", "2", "3"), type = "Tm")
  expect_equal(unname(tm), c(1, 2, 3))


  # ---- Error checks ----
  # Tm rejects out-of-range values
  expect_error(
    tryCatch(ValInput(c(25, 1, 2), type = "Tm"))
  )

})
