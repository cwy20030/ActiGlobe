test_that("ggActiGlobe returns a ggplot object with expected structure", {

  BdfList <- BriefSum(
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-24 13:45:00",
    TZ = "America/New_York"
  )

  p <- ggActiGlobe(
    df = BdfList$df,
    Bdf = BdfList$Bdf,
    VAct = "Activity",
    VDT = "DateTime"
  )

  # ---- Structure checks ----
  # Object should be a ggplot
  expect_s3_class(p, "ggplot")

  # ---- Relationship checks ----
  # Midnight vertical line should be present
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% geoms)

  # ---- Content checks ----
  # Axis labels should match expected values
  expect_equal(p$labels$y, "Activity Count")
  expect_true(p$labels$x %in% "Date")


})
