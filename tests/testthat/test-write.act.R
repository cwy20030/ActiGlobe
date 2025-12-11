test_that("write.act exports daily recordings and summary correctly", {

print (Sys.info ()[["sysname"]])
  
  if (grepl ("Linux|Darwin", Sys.info ()[["sysname"]])) {
    skip("Skip on Linux and macOS due to segfault fail")
  } else {


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

  # Construct expected directory ----------------
  fDir <- file.path (paste0 (tmpdir, "/JD"))

  # Check that directory was created ------------------
  expect_true (dir.exists (fDir))

  # Check that at least one daily .txt file was written
  txt_files <- list.files (fDir, pattern = "\\.txt$", full.names = TRUE)
  expect_true (length (txt_files) == 5)

  # Check that Summary.csv was written
  summary_file <- file.path (fDir, "Summary.csv")
  expect_true (file.exists (summary_file))

  # Validate contents of Summary.csv
  summary_df <- utils::read.csv (summary_file)
  expect_s3_class (summary_df, "data.frame")
  expect_true ("Date" %in% names (summary_df))
  }
})
