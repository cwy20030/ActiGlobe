test_that ("Geo2TZ returns expected structure
(mocked interactive +
           mocked HTTP + mocked translation)", {

  skip_if_not_installed ("mockery")


  # Fake Nominatim response (2 hits) --------------------------------------
  nominatim_json <- jsonlite::toJSON (list (
    list (
      display_name = "Paris, Île-de-France, France",
      lat = "48.8566",
      lon = "2.3522"
    ),
    list (
      display_name = "Paris, Texas, United States",
      lat = "33.6609",
      lon = "-95.5555"
    )
  ), auto_unbox = TRUE)


  # Mock readline: query -> choose #1 -------------------------------------
  i <- 0
  fake_readline <- function (prompt = "") {
    i <<- i + 1
    if (i == 1) return ("paris")
    if (i == 2) return ("1")
    ""
  }

  # Dummy response object from req_perform --------------------------------
  dummy_resp <- list (ok = TRUE)

  # Mock req_perform: return dummy response for Nominatim ------------------
  fake_req_perform <- function (req, ...) {
    dummy_resp
  }

  # Stub resp_check_status: do nothing ------------------------------------
  fake_resp_check_status <- function (resp, ...) {
    invisible (resp)
  }

  # Stub resp_body_string: return JSON for Nominatim -----------------------
  fake_resp_body_string <- function (resp, ...) {
    nominatim_json
  }


  fake_detect_language <- function(Text, ...) "en"


  # ---- Relationship checks ----
  # Stub test for interactive readline and HTTP requests
  mockery::stub(Geo2TZ, "readline", fake_readline)
  mockery::stub(Geo2TZ, "httr2::req_perform", fake_req_perform)
  mockery::stub(Geo2TZ, "httr2::resp_check_status", fake_resp_check_status)
  mockery::stub(Geo2TZ, "httr2::resp_body_string", fake_resp_body_string)
  mockery::stub(Geo2TZ, "DetectLanguage", fake_detect_language)

  # NEW: mock Translator() so Suggest() does not try to detect language
  fake_translator <- function(Text, Lang = NULL, ...) Text
  mockery::stub(Geo2TZ, "Translator", fake_translator)

  # Fake Nominatim response (2 hits)
  nominatim_json <- jsonlite::toJSON(list(
    list(
      display_name = "Paris, Île-de-France, France",
      lat = "48.8566",
      lon = "2.3522"
    ),
    list(
      display_name = "Paris, Texas, United States",
      lat = "33.6609",
      lon = "-95.5555"
    )
  ), auto_unbox = TRUE)

  # Fake readline: first "paris", then "1"
  i <- 0
  fake_readline <- function(prompt = "") {
    i <<- i + 1
    if (i == 1) return("paris")
    if (i == 2) return("1")
    ""
  }

  # Dummy HTTP mocks
  dummy_resp <- list (ok = TRUE)
  fake_req_perform <- function (req, ...) dummy_resp
  fake_resp_check_status <- function (resp, ...) invisible (resp)
  fake_resp_body_string <- function (resp, ...) nominatim_json
  fake_detect_language <- function (Text, ...) "fr"



  # Apply stubs BEFORE calling Geo2TZ()
  mockery::stub (Geo2TZ, "readline", fake_readline)
  mockery::stub (Geo2TZ, "httr2::req_perform", fake_req_perform)
  mockery::stub (Geo2TZ, "httr2::resp_check_status", fake_resp_check_status)
  mockery::stub (Geo2TZ, "httr2::resp_body_string", fake_resp_body_string)
  mockery::stub (Geo2TZ, "DetectLanguage", fake_detect_language)

  # NEW: mock Translator() so Suggest() does not try to detect language
  fake_translator <- function (Text, Lang = NULL, ...) Text
  mockery::stub (Geo2TZ, "Translator", fake_translator)

  # Run Geo2TZ() and capture printed output
  out_txt <- capture.output (
    out <- suppressWarnings (Geo2TZ ())
  )

  # ---- Structure checks ----
  expect_true (is.list (out))
  expect_named (out, c ("Picked", "Offset"))

  expect_true (is.data.frame (out$Picked))
  expect_named (out$Picked, c ("Label", "Lat", "Lon", "TZ"))

  expect_true (is.list (out$Offset))
  expect_true (all (c ("TZ", "Standard", "DST", "Current", "Abbrev") %in%
                    names (out$Offset)))

  # timezone is IANA-like or UTC fallback
  expect_true (grepl ("/", out$Picked$TZ) || out$Picked$TZ %in% c ("UTC"))
  # ---- Output text checks ----
  expect_true (any (grepl ("Time difference to UTC\\+0", out_txt)))
  expect_true (any (grepl ("Standard time zone:", out_txt)))
  expect_true (any (grepl ("Daylight saving time:", out_txt)))
  expect_true (any (grepl ("Current time zone offset:", out_txt)))
  expect_true (any (grepl ("Time zone abbreviation:", out_txt)))
})
