test_that ("Translator translates on success between langauges", {

  skip_if_not_installed ("mockery")

  # lightweight check wifi
  skip_if_not(curl::has_internet())   # skip when offline

  x.ES  <- "Hola, mundo!"
  x.ES2 <- "¡Hola, mundo!"
  x.Eng <- "Hello, world!"

  out.ES  <- Translator (Text = x.ES, Lang = "en", Timeout = 1000)
  out.ES2 <- Translator (Text = x.ES2, Lang = "en")
  out.Eng <- Translator (Text = x.Eng, Lang = "es")

  # ---- Structure checks ----
  # Check that the output is a character string
  expect_type (out.ES, "character")
  expect_type (out.Eng, "character")

  expect_equal (nchar (out.ES), 13L)
  expect_equal (length (out.Eng), 1L)

  # ---- Relationship checks ----
  # Check matches in translation between Spanish and English
  expect_equal (out.ES, x.Eng)
  expect_equal (out.Eng, x.ES2)


  # ---- Content checks ----
  # Expected variance values
  expect_equal (out.ES, "Hello, world!")

  # ---- Error checks ----
  # Wrong type of object should trigger an error
  expect_error (Translator (Text = 123, Lang = "en"))

  # Empty string should trigger a stop
  expect_error (Translator (Text = "", Lang = "en"))


  # Should fail due to punctuation difference)
  expect_failure (expect_equal (out.ES, out.ES2))

  # Fail due to zero time-out
  expect_error (Translator (Text = x.Eng, Lang = "es", Timeout = 0))
})



test_that ("LangDetect returns expected values (mocked cld3)", {
  skip_if_not_installed ("mockery")

  fake_detect_language <- function (Text) "en"
  mockery::stub (LangDetect, "cld3::detect_language", fake_detect_language)

  # ---- Content checks ----
  expect_equal (LangDetect ("Hello, world!"), "en")

  # ---- Error checks ----
  expect_error (expect_failure (LangDetect ("")))
  expect_error (expect_failure (LangDetect ("   ")))
  expect_error (expect_failure (LangDetect ("???")))
  expect_error (expect_failure (LangDetect ("12345")))
})


