# File ActiGlobe/R/TZwDST.R
#
# Copyright (C) 2025 C. William Yao, PhD
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.
#
#' @title Find DST Start and End Date/Time for IANA Time Zones
#'
#' @description
#' `TZwDST` identifies the start and end date/time of daylight saving
#' time (DST) for a given set of IANA time zones and a specified year.
#' It returns a data frame summarizing the DST transitions and UTC offsets
#' for each time zone.
#'
#' @importFrom stats setNames
#'
#' @param TZ A character vector of time zone identifiers.
#' @param Year Integer. Defaults to current system year if NULL.
#' @param Simple Logical. If TRUE (default), returns a logical vector
#' indicating whether DST occurs in each time zone for the specified year.
#' If FALSE, returns a detailed data frame with DST transition information.
#'
#' @returns
#' Two possible outputs based on the `Simple` parameter:
#'
#' A logical vector of the same length as `TZ`.
#'   \itemize{
#'     \item `TRUE` Daylight saving time occurs during the year.
#'     \item `FALSE` No daylight saving time during the year.
#' }
#'
#'  A data.frame with one row per time zone and columns:
#' \itemize{
#'   \item Information on Daylight Saving Time Transition
#'   \itemize{
#'    \item TZ_NAME: Time zone name.
#'    \item UTC_DST: UTC offset during DST (e.g., "UTC -04:00").
#'    \item DST_Start_Date: Date when DST starts (NA if no DST).
#'    \item DST_Start_Time: Time when DST starts (NA if no DST).
#'    \item UTC_Standard: UTC offset during standard time (e.g., "UTC -05:00").
#'    \item DST_End_Date: Date when DST ends (e.g, "2021-11-07", NA if no DST).
#'    \item DST_End_Time: Time when DST ends (e.g., "02:00:00", NA if no DST).
#'    }
#'   \item Time Zone Shift (Special Cases)
#'   \itemize{
#'    \item Original_UTC: UTC offset before day jump* or time zone shift or
#'    realignment (e.g., "Australia/LHI"  in 1981).
#'    \item Shift_Start_Date: Date when the shift starts.
#'    \item Shift_Start_Time: Time when the shift starts.
#'    \item New_UTC: UTC offset after the shift.
#'    \item New_Date: Date after the shift.
#'    \item New_Time: Time after the shift.
#'    }
#' }
#'
#' @seealso
#' \code{\link{OlsonNames}}
#'
#' @examples
#' # Find DST transitions for a few time zones in 2021
#' TZwDST(TZ = c("America/New_York", "Europe/London
#' ", "Asia/Tokyo"), Year = 2021)
#'
#' TZwDST(TZ = "Africa/Cairo", Year = 2010)
#'
#' \donttest{
#' # Find DST transitions for all time zones in the current year
#' TZwDST(TZ = OlsonNames()) # This will take a while
#' }
#'
#' @keywords tz dst year
#' @export

TZwDST <- function (TZ, Year = NULL, Simple = TRUE) {
  # Step 0. Input Validation and Parameter Extraction ------
  TZ   <- ValInput (TZ, type = "TZ")

  if (is.null (Year)) {
    Year <- as.integer (format (Sys.Date (), "%Y"))
  } else {
    Year <- as.numeric (Year)
  }


  # Step 1. Identify Transition Time ------
  DJ.DST <- lapply (TZ, function (Tz) {
    Temp <- WhenTransit (TZ = Tz, Year = Year, Internal = TRUE)
    Ln   <- Temp$Ln

    if (Simple) {
      x <- na.omit (c (Temp$DST$DST$Date, Temp$DST$ST$Date))
      stats::setNames (length (x) > 0, Tz)

    } else {
      dfDST <- data.frame (
        TZ_NAME        = rep (Tz, Temp$DST$Ln),
        UTC_DST        = Temp$DST$DST$UTC,
        DST_Start_Date = Temp$DST$DST$Date,
        DST_Start_Time = Temp$DST$DST$Time,
        UTC_Standard   = Temp$DST$ST$UTC,
        DST_End_Date   = Temp$DST$ST$Date,
        DST_End_Time   = Temp$DST$ST$Time,
        stringsAsFactors = FALSE
      )

      dfDJ <- data.frame (
        TZ_NAME          = rep (Tz, Temp$DJ$Ln),
        Original_UTC     = Temp$DJ$From$UTC,
        Shift_Start_Date = Temp$DJ$From$Date,
        Shift_Start_Time = Temp$DJ$From$Time,
        New_UTC          = Temp$DJ$To$UTC,
        New_Date         = Temp$DJ$To$Date,
        New_Time         = Temp$DJ$To$Time,
        stringsAsFactors = FALSE
      )

      merge.data.frame (dfDST, dfDJ, by = "TZ_NAME", all = TRUE)
    }
    })


  # Step 2. Return Values ---------
  if (Simple) {
    Out <- unlist (DJ.DST)
  } else {
  Out <- do.call (rbind, DJ.DST)
  }


  return (Out)
}
