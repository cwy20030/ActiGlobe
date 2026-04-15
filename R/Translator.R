#' @title Text Translation
#'
#' @description
#' Translate a text string to a target language using MyMemory Translation API.
#' Source language is detected locally using \code{LangDetect}. If detection or
#' translation fails, the original text is returned.
#'
#' @importFrom curl has_internet
#' @importFrom httr2 request req_url_query req_user_agent req_timeout
#' @importFrom httr2 req_perform resp_check_status resp_body_string resp_status
#' @importFrom jsonlite fromJSON
#'
#' @param Text The text string to be translated.
#' @param Lang Target language code (e.g., "en", "es", "fr"). If NULL or empty,
#' no translation is performed.
#' @param Timeout Request timeout in seconds. Default = 20.
#' @param TR_Base Translation endpoint for MyMemory (default provided).
#'
#' @returns Translated text string, or original text if translation fails.
#'
#' @examples
#' # Translate a label to Spanish
#' Translator ("Hello, world!", Lang = "es")
#'
#' @noRd



Translator <-
  function (Text,
            Lang = "en",
            Timeout = 20,
            TR_Base = "https://api.mymemory.translated.net/get") {

    # Step 0 Check Requirement -------------
    ## Check internet connection
    if (!curl::has_internet ()) {
      stop ("No internet connection!
            Please check the connection and try again.")
    }

    if (is.null (Lang) || !nzchar (Lang)) return (Text)


    # Step 1 Detect source language locally (cld3) -----------------------
    Src <- LangDetect (Text)

    ## MyMemory does NOT accept "auto|xx"
    ## fallback if detection is unknown
    if (is.null (Src) || !nzchar (Src)) Src <- "en"

    ## Avoid unnecessary translation if already in target language
    if (tolower (Src) == tolower (Lang)) return (Text)

    # Step 2 Request translation -----------------------------
    Req <-
      httr2::request (TR_Base) |>
      httr2::req_url_query (
        q = Text,
        langpair = paste0 (Src, "|", Lang)
      ) |>
      httr2::req_timeout (Timeout)

    Resp <-
      tryCatch (
        httr2::req_perform (Req),
        error = function (e) {NULL}
      )

    if (is.null (Resp)) return (Text)
    if (httr2::resp_status (Resp) >= 300) return (Text)

    Txt <- httr2::resp_body_string (Resp)

    # Step 3 Prepare Output ------------------
    ##  Extract translated text from JSON object
    X <-
      tryCatch (
        jsonlite::fromJSON (Txt),
        error = function (e) {NULL}
      )

    if (is.null (X)) return (Text)
    if (!("responseData" %in% names (X))) return (Text)
    if (!("translatedText" %in% names (X$responseData))) return (Text)

    TText <- X$responseData$translatedText

    ## Check for error message mistakenly passed as translatedText
    if (!is.character (TText) || length (TText) != 1) return (Text)
    if (!nzchar (TText)) return (Text)
    if (grepl ("INVALID SOURCE LANGUAGE", toupper (TText))) return (Text)

    TText
  }



#' @title Language Detection
#'
#' @description
#' Detect the language of a given text string using `cld3`. If detection fails
#' or returns "und" (undetermined), NA is returned.
#'
#' @importFrom cld3 detect_language
#'
#' @param Text The text string to be translated.
#'
#' @returns Detected language code (e.g., "en", "es"), or NA if detection fails.
#'
#' @examples
#' # Detect language of a text
#' LangDetect ("Hello, world!")
#'
#' @noRd

LangDetect <- function (Text) {

  if (is.null (Text) || !nzchar (trimws (Text))) return (NA_character_)

  L <- cld3::detect_language (Text)

  if (!nzchar (L) || L %in% c ("und", "unknown")) return (NA_character_)

  L
}
