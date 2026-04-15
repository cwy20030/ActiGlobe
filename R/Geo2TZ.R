#' @title Interactive Geological Location to TZ Offset Search Portal
#'
#' @description
#' `Geo2TZ` provides an interactive prompt to search a place and
#' return its time-zone offsets (standard, DST delta, current) and abbreviation.
#' Note that internet is required for this process.
#'
#' @details
#' Search is performed using OpenStreetMap Nominatim (no API key), mimicking
#' the typical search experience. Time zone is resolved using the geological
#' coordinates. Offsets and abbreviation are derived from the local time zone
#' database. Note that time zone abbreviations may vary by the local operating
#' system.
#'
#'
#' @param Date A POSIXct time used to compute "current" offset. Note that
#' when processing location with daylight saving time, it is crucial to be
#' specific about the date. Default uses \code{Sys.time}
#' @param Translate Logical; whether to translate suggestion labels
#' (default = FALSE).
#' @param Lang Target language code for translated place labels (e.g., "es",
#' "fr", "zh"). Default NULL means no translation.
#' @param Limit Maximum number of suggestions returned per query (default = 8).
#' @param Timeout Request timeout in seconds (default = 20).
#' @param UA User agent string required by Nominatim (default provided;
#' you should include a contact identifier).
#'
#' @returns
#' Invisibly returns a named list with elements:
#' \itemize{
#'   \item Picked a data.frame with `Label`, `Lat`, `Lon`, `TZ`
#'   \item Offset a list with `Standard`, `DST`, `Current`, `Abbrev`, `TZ`
#' }
#'
#' @examples
#' \dontrun{
#'
#'   Geo2TZ ()
#'
#'   # Try typing Taipei after running the following code
#'   Geo2TZ (Translate = FALSE, Lang = "es")
#'
#' }
#'
#' @export

Geo2TZ <- function (Date = Sys.time (),
                    Translate = TRUE,
                    Lang = "en",
                    Limit = 8, Timeout = 20,
                    UA = NULL) {

  # Step 0 Check Requirement -------------
  ## Check internet connection
  if (!curl::has_internet ()) {
    stop ("No internet connection! Please check the connection and try again.")
  }

  ## Check User Agent string

  if (is.null (UA) || !nzchar (UA)) {
    UA <- Default_UA ()
  }


  # Step 1 Interactive query loop ----------------------------------
  repeat {
    ## Prompt user for a place name or address
    Q <- readline ("Search for a place (press Enter to quit): ")
    Q <- trimws (Q)

    if (!nzchar (Q)) return (invisible (NULL))

    ## Get suggestions based on the query
    Hits <- Suggest (Query = Q,
                     UA = UA,
                     Limit = Limit,
                     Timeout = Timeout,
                     Translate = Translate,
                     Lang = Lang)

    if (!is.data.frame (Hits) || nrow (Hits) == 0) {
      cat ("\nNo results. Try a different query.\n\n")
      next
    }

    cat ("\nSuggestions:\n")
    for (i in seq_len (nrow (Hits))) {
      cat (paste0 ("[", i, "] ", Hits$Label [i], "\n"))
    }

    Ans <- readline (paste0 ("Choose 1-", nrow (Hits),
                             " (Enter to search again): "))
    Ans <- trimws (Ans)

    if (!nzchar (Ans)) {
      cat ("\n")
      next
    }

    ## Validate user input
    Idx <- suppressWarnings (as.integer (Ans))

    if (is.na (Idx) || Idx < 1 || Idx > nrow (Hits)) {
      cat ("\nInvalid selection.\n\n")
      next
    }

    Pick <- Hits [Idx, ]

    ## Convert coordinates to time zone offsets
    Off <- Coord2Offset (Lat = Pick$Lat, Lon = Pick$Lon)

    cat ("\nSelected:\n")
    cat (Pick$Label, "\n\n")

    cat ("Time difference to GMT/UTC\n")
    cat (paste0 ("Standard time zone:\t", Off$Standard, "\n"))
    cat (paste0 ("Daylight saving time:\t", Off$DST, "\n"))
    cat (paste0 ("Current time zone offset:\t", Off$Current, "\n"))
    cat (paste0 ("Time zone abbreviation:\t", Off$Abbrev, "\n\n"))

    Pick$TZ <- Off$TZ

    return (invisible (list (
      Picked = Pick,
      Offset = Off
    )))
  }
}





#' @title Internal helper function for formatting UTC and DST offsets
#'
#' @param Sec Time in seconds
#' @param type Character string indicating the type of offset ("UTC" or "DST")
#'
#' @noRd

Offset_Labeler <- function (Sec, type = c("UTC","DST")) {

  if (is.na (Sec)) return (NA_character_)

  Hr <- Sec / 3600
  HrA <- abs (Hr)

  Sgn <- ifelse (Hr >= 0, "+", "-")
  Sgn <- ifelse (type == "UTC", paste0 ("UTC/GMT ", Sgn),
                 ifelse (type == "DST", Sgn, NA))

  if (abs (HrA - round (HrA)) < 1e-9) {
    paste0 (Sgn, round (HrA), " hour",
            ifelse (round (HrA) == 1, "", "s"))
  } else {
    x <- sub ("\\.?0+$", "", format (HrA, trim = TRUE))
    paste0 (Sgn, x, " hours")
  }
}









#' @title Convert Coordinates to Time Zone Offsets
#'
#' @importFrom lutz tz_lookup_coords
#'
#' @param Lat Latitude of the location.
#' @param Lon Longitude of the location.
#' @param Date A POSIXct time used to compute offset. Note that when processing
#' location with daylight saving time, it is crucial to be specific about the
#' date. Default uses \code{Sys.time}.
#'
#' @noRd

Coord2Offset <- function (Lat, Lon, Date = Sys.time ()) {

  TZ <-
    tryCatch (
      lutz::tz_lookup_coords (lat = Lat, lon = Lon, method = "accurate"),
      error = function (e) {"UTC"}
    )

  Now_UTC <- as.POSIXct (Date, tz = "UTC")

  Now_Loc <- as.POSIXlt (Now_UTC, tz = TZ)
  Cur_Sec <- as.integer (Now_Loc$gmtoff)

  Yr      <- as.integer (format (Now_UTC, "%Y"))
  Win_UTC <- as.POSIXct (paste0 (Yr, "-01-15 12:00:00"), tz = "UTC")
  Win_Loc <- as.POSIXlt (Win_UTC, tz = TZ)
  Std_Sec <- as.integer (Win_Loc$gmtoff)

  DST_Sec <- Cur_Sec - Std_Sec

  Abb <-
    tryCatch (
      format (as.POSIXct (Now_UTC, tz = TZ), "%Z"),
      error = function (e) {NA_character_}
    )

  list (
    TZ       = TZ,
    Standard = Offset_Labeler (Std_Sec, type = "UTC"),
    DST      = Offset_Labeler (DST_Sec, type = "DST"),
    Current  = Offset_Labeler (Cur_Sec, type = "UTC"),
    Abbrev   = Abb
  )
}
