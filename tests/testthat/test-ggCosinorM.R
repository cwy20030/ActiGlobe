test_that ("ggCosinorM returns a ggplot object with expected structure", {
   BdfList <- BriefSum (
      df = FlyEast,
      SR = 1 / 60,
      Start = "2017-10-24 13:45:00"
   )

   # Extract actigraphy data from a single day -------------------------------
   df <- BdfList$df
   df <- subset (df, df$Date == "2017-10-28")

   fit <- CosinorM (
      time = df$Time,
      activity = df$Activity,
      tau = 24,
      method = "OLS"
   )

   p <- ggCosinorM (
      object = fit,
      labels = TRUE,
      ci = TRUE,
      ci_level = 0.95,
      title_extra = "2017-10-24"
   )

   # ---- Structure checks ----
   # Object should be a ggplot
   expect_s3_class (p, "ggplot")

   # ---- Relationship checks ----
   # Geoms should include model fit line, confidence ribbon, and observed points
   geoms <- vapply (p$layers, function (l) class (l$geom)[1], character (1))
   expect_true ("GeomLine" %in% geoms)
   expect_true ("GeomRibbon" %in% geoms)
   expect_true ("GeomPoint" %in% geoms)

   # ---- Content checks ----
   # Labels should include title and axes
   expect_equal (p$labels$x, "Time")
   expect_equal (p$labels$y, "Activity")
   expect_true (grepl ("2017-10-24", p$labels$title))

   # Colour scale should be defined
   scales <- vapply (p$scales$scales, function (s) class (s)[1], character (1))
   expect_true (any (grepl ("ScaleDiscrete", scales)))

   # ---- Error checks ----
   # Invalid inputs should trigger errors
   expect_error (tryCatch (
      ggCosinorM (object = fit,
                  ci_level = 1.5)
   ))
})
