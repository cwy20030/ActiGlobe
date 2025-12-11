test_that("write.cosinor exports PDF and summary CSV correctly", {

  # Create a temporary directory for testing ------------------
  tmpdir <- tempdir ()

  ID = "TESTED"

  BdfList <-
    BriefSum (
      df = FlyEast,
      SR = 1 / 60,
      Start = "2017-10-24 13:45:00",
      TZ = "America/New_York"
    )

  # Let's extract the quick summary of the recording
  Bdf <- BdfList$Bdf
  df <- BdfList$df


  Bdf <- Bdf [5:8, ]


dfList <-
     Act2Daily (
         df = df,
         Bdf = Bdf,
         VAct = "Activity",
         VTm = "Time"
     )


# OLS ----------------------------------------------------
write.cosinor (
    Dir = tmpdir, ## Export to the current working directory
    ID = ID,
    DailyAct = dfList$Daily_df,
    Bdf = Bdf,
    VAct = "Activity",
    VTm = "Time"
)


# ---- Structure checks ----
fDir <- file.path(paste0(tmpdir,"/", ID,"/"))
expect_true(dir.exists(fDir))   # directory exists

pdfFile <- file.path(fDir, paste0(ID, ".pdf"))
expect_true(file.exists(pdfFile))   # PDF exists

csvFile <- file.path(fDir, "Summary.csv")
expect_true(file.exists(csvFile))   # CSV exists

# ---- Relationship checks ----
# Verify that the summary CSV relates correctly to the input Bdf
out <- utils::read.csv(csvFile, stringsAsFactors = FALSE)
expect_equal(nrow(out), nrow(Bdf))   # same number of rows as input subset
expect_true(length(out) > length(Bdf))

# ---- Content checks ----
# Ensure the summary CSV contains expected cosinor coefficient columns
expect_true(all(c("MESOR", "Amplitude", "Acrophase", "Acrophase.time") %in% names(out)))



# KDE --------------------------------------------------------------
## with  overwrite ----------------
write.cosinor (
  Dir = tmpdir, ## Export to the current working directory
  ID = ID,
  DailyAct = dfList$Daily_df,
  Bdf = Bdf,
  VAct = "Activity",
  VTm = "Time",
  overwrite = TRUE
)


# ---- Structure checks ----
fDir <- file.path(paste0(tmpdir,"/", ID,"/"))
expect_true(dir.exists(fDir))   # directory exists

pdfFile <- file.path(fDir, paste0(ID, ".pdf"))
expect_true(file.exists(pdfFile))   # PDF exists

csvFile <- file.path(fDir, "Summary.csv")
expect_true(file.exists(csvFile))   # CSV exists

# ---- Relationship checks ----
# Verify that the summary CSV relates correctly to the input Bdf
out <- utils::read.csv(csvFile, stringsAsFactors = FALSE)
expect_equal(nrow(out), nrow(Bdf))   # same number of rows as input subset
expect_true(length(out) > length(Bdf))

# ---- Content checks ----
# Ensure the summary CSV contains expected cosinor coefficient columns

expect_false(all(c ("MESOR", "Bathyphase.time", "Trough.ph", "Acrophase.time", "Peak", "Amplitude")
 %in% names(out)))




## with  ph and overwrite ----------------
write.cosinor (
  Dir = tmpdir, ## Export to the current working directory
  ID = ID,
  DailyAct = dfList$Daily_df,
  Bdf = Bdf,
  VAct = "Activity",
  VTm = "Time",
  ph = TRUE,
  overwrite = TRUE
)


# ---- Structure checks ----
fDir <- file.path(paste0(tmpdir,"/", ID,"/"))
expect_true(dir.exists(fDir))   # directory exists

pdfFile <- file.path(fDir, paste0(ID, ".pdf"))
expect_true(file.exists(pdfFile))   # PDF exists

csvFile <- file.path(fDir, "Summary.csv")
expect_true(file.exists(csvFile))   # CSV exists

# ---- Relationship checks ----
# Verify that the summary CSV relates correctly to the input Bdf
out <- utils::read.csv(csvFile, stringsAsFactors = FALSE)
expect_equal(nrow(out), nrow(Bdf))   # same number of rows as input subset
expect_true(length(out) > length(Bdf))

# ---- Content checks ----
# Ensure the summary CSV contains expected cosinor coefficient columns

expect_true(all(c ("MESOR", "Bathyphase.time", "Trough.ph", "Acrophase.time", "Peak", "Amplitude")
                 %in% names(out)))


})
