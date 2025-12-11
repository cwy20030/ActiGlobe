test_that("write.cosinor exports PDF and summary CSV correctly", {


  # Create a temporary directory for testing ------------------
  tmpdir <- tempdir()

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

  print (0)

dfList <-
     Act2Daily (
         df = df,
         Bdf = Bdf,
         VAct = "Activity",
         VTm = "Time"
     )


print (1)
write.cosinor (
    Dir = tmpdir, ## Export to the current working directory
    ID = ID,
    DailyAct = dfList$Daily_df,
    Bdf = Bdf,
    VAct = "Activity",
    VTm = "Time"
)


# ---- Structure checks ----
  print (2)

fDir <- file.path(paste0(tmpdir,"/", ID,"/"))
expect_true(dir.exists(fDir))   # directory exists
  print (3)
pdfFile <- file.path(fDir, paste0(ID, ".pdf"))
expect_true(file.exists(pdfFile))   # PDF exists
  print (4)
csvFile <- file.path(fDir, "Summary.csv")
expect_true(file.exists(csvFile))   # CSV exists
})

