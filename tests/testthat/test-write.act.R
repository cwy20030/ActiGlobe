test_that("write.act exports daily recordings and summary correctly", {

print(dir.exists("~/Documents"))



  # Create a temporary directory for testing ------------------
  tmpdir <- getwd ()

  BdfList <-
    BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

  # Let's extract the quick summary of the recording
  Bdf <- BdfList$Bdf
  df <- BdfList$df


  Bdf <- Bdf [1:5, ]

print("prep")
 # Call write.act ------------------
write.act (
  Dir = tmpdir, ## Export to the current working directory
  ID = "JD",
  df = df,
  Bdf = Bdf,
  VAct = "Activity",
  VTm = "Time",
  Incomplete = TRUE,
  Travel = TRUE,
  Simple = FALSE
)
print (0)
  # Construct expected directory ----------------
  fDir <- file.path (paste0 (tmpdir, "/JD"))
print (1)
  # Check that directory was created ------------------
  expect_true (dir.exists (fDir))

  # Check that at least one daily .txt file was written
  txt_files <- list.files (fDir, pattern = "\\.txt$", full.names = TRUE)
  expect_true (length (txt_files) == 5)

  # Check that Summary.csv was written
  summary_file <- file.path (fDir, "Summary.csv")
  expect_true (file.exists (summary_file))


})
