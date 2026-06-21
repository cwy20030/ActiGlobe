#  File ActiGlobe/R/TZ2UTC.R
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
#' @title
#' Compute UTC offset based on Time Zone and Date
#'
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom lifecycle deprecate_soft badge
#'
#' @param Date A POSIXct date used as the reference point for offset
#'   comparison. Defaults to January 1, 2021 UTC if `NULL`.
#' @param TZ The time zone when the recording started. Default = "local",
#' which means user's local time zone
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
#'
#' @return A character string indicating the UTC offset, e.g., "UTC-05:00"
#'
#'
#' @examples
#' x <- as.Date(c("2017-10-24", "2017-11-20"))
#' TZ2UTC(Date = x, TZ = "America/New_York") ## Expect two different UTCs
#'
#'
#' # A vector of dates
#' x <- seq.Date(from = as.Date("2017-03-15"),
#'               to   = as.Date("2017-11-27"),
#'               by   = "day")
#'
#'
#' # If the user resides in a country that follows daylight saving time
#' TZ2UTC(Date = x, TZ = "local")
#'
#' \dontrun{
#' # If not
#'  TZ2UTC(Date = x, TZ = "America/New_York")
#' }
#'
#' @keywords UTC TZ
#'
#'
#' @export

TZ2UTC <- function (Date = NULL, TZ = "local", DT = NULL) {
    # Step 0. Input Validation and Parameter Extraction -----------
    if (is.null (Date) && is.null (DT)) {
        Date <- as.Date ("2021-01-01")
    }
    if (is.null (Date) && !is.null (DT)) {
        lifecycle::deprecate_soft (
            when = "0.3.1",
            what = "TZ2UTC(DT)",
            with = "TZ2UTC(Date)"
        )
        Date <- DT
    }

    ## Process TZ
    if (TZ == "local") TZ <- Sys.timezone ()

    ## Validate the time zone
    valid_zones <- OlsonNames ()
    if (!TZ %in% valid_zones) {

        sIANA <- mIANA ()

        if (any(TZ %in% sIANA$TZ_Code)) {
            TZ <- sIANA$Timezone_IANA [match (TZ, sIANA$TZ_Code)]
        } else {
            stop (sprintf (
                "The provided time zone \"%s\" is not recognized.\n",
                TZ
            ), "Please check the spelling or consult the IANA time zone table
    (ActiGlobe::IANA).")
        }
    }


    Date <- as.Date (Date)
    x <- lubridate::ymd_hms (paste0 (Date, " 12:00:00"), tz = TZ)
    y <- lubridate::with_tz (x, tzone = "UTC")

    a <- as.POSIXct (format (x, format = "%H:%M"), format = "%H:%M")
    b <- as.POSIXct (format (y, format = "%H:%M"), format = "%H:%M")

    ab <- a - b ### Compute time difference
    ab <- as.numeric (ab, units = "hours") ### Convert to numeric


    ## Get the hours
    c <- ifelse (ab < 0, ceiling (ab), floor (ab)) ### Hour unit
    d2 <- (abs (ab) - abs (c)) * 60 ### Minute unit

    mp <- ifelse (ab < 0, "-", "+") ### Check positive or negative

    e <- ifelse (abs (c) < 10, paste0 (mp, "0", abs (c)), paste0 (mp, c))
    f <- ifelse (d2 == 0, "00", d2)


    Out <- paste0 ("UTC", e, ":", f)


    return (Out)
}
