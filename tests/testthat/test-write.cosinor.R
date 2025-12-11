test_that("write.cosinor exports PDF and summary CSV correctly", {
  # Create a temporary directory for testing ------------------
  tmpdir <- getwd ()

  ID = "TESTED"

  BdfList <-
    BriefSum (
      df = FlyEast,
      SR = 1 / 60,
      Start = "2017-10-24 13:45:00"
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



write.cosinor (
    Dir = tmpdir, ## Export to the current working directory
    ID = ID,
    DailyAct = dfList$Daily_df,
    Bdf = Bdf,
    VAct = "Activity",
    VTm = "Time"
)


# ---- Structure checks ----
print(tmpdir)

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

# ---- Content checks ----
# Ensure the summary CSV contains expected cosinor coefficient columns
expect_true(all(c("MESOR", "Amplitude", "Acrophase", "Acrophase.time") %in% names(out)))

# ---- Error checks ----
# Confirm that invalid tau values trigger an error in Rad2Hr (used internally)
expect_error(Rad2Hr(1, 0), "tau must be greater than 0")
expect_error(Rad2Hr(1, 25), "tau must be greater than 0")



})
