#  File ActiGlobe/R/Translator.R
#
#  Copyright (C) 2025  C. William Yao, PhD
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Affero General Public License as
#  published by the Free Software Foundation, either version 3 of the
#  License, or any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Affero General Public License for more details.
#
#  You should have received a copy of the GNU Affero General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
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

Translator <- function (Text,
                        Lang = "en",
                        Timeout = 20,
                        TR_Base = "https://api.mymemory.translated.net/get") {

    # Step 0 Check Requirement -------------
    ## Check internet connection
    if (!curl::has_internet ()) {
      stop ("No internet connection!
            Please check the connection and try again.")
    }

    if (any (is.null (Lang), !nzchar (Lang))) return (Text)

    if (Text == "")
      stop ("Input text is empty!
            Please provide a non-empty string for translation.")
    # Step 1 Detect source language locally (cld3) -----------------------
    Src <- LangDetect (Text)

    Src <- .LDCheck (Text, Src)

    ## Avoid unnecessary translation if already in target language
    if (tolower (Src) == tolower (Lang)) {
      return (Text)
    } else {

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

      if (any (is.null (X),
               !("responseData" %in% names (X)),
               !("translatedText" %in% names (X$responseData))))
        return (Text)

      TText <- X$responseData$translatedText

      ## Check for error message mistakenly passed as translatedText
      if (any (!is.character (TText),
               length (TText) != 1,
               !nzchar (TText),
               grepl ("INVALID SOURCE LANGUAGE", toupper (TText)))) {
        return (Text)

      } else {

        return (TText)

      }
    }
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

  if (is.null (Text) || !nzchar (trimws (Text)))
    return (NA_character_)

  L <- cld3::detect_language (Text)

  if (!nzchar (L) || L %in% c ("und", "unknown"))
    return (NA_character_)

  L
}



.LDCheck <- function (Text, Src) {
  if (is.na (Src)) {
    txt_clean <- gsub ("\\(|\\)", ", ", Text)
    txt_clean <- strsplit (txt_clean,",| ") [[1]]
    txt_clean <- txt_clean [!txt_clean == ""]
    Src <- vapply (txt_clean, function (x) {
      LangDetect (x)
    }, FUN.VALUE = character (1L))
    Src <- Src [!is.na (Src)] [1L]
    warning ("Language detection failed for the full text.
               Attempting detection on the first text components.")
  }


  ## MyMemory does NOT accept "auto|xx"
  ## fallback if detection is unknown
  if (any (is.null (Src), !nzchar (Src))) Src <- "en"

 return (Src)
}
