#  File ActiGlobe/R/DST.R
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
#' @title Daylight Saving Time
#'
#' @description
#' Determine if Dates Fall in Daylight Saving Time for the Given Time Zone(s)
#'
#' @details
#' For each element of `Date`, returns `TRUE` if that date-time is in
#' daylight saving time (DST) under the specified time zone.  The
#' function handles transitions by detecting the "gain" or "loss" of
#' hours via `DST2GL()` and flipping the DST flag on the transition day.
#'
#' @importFrom lifecycle badge
#'
#' @param Date A vector of class `Date`, `POSIXct`, or `POSIXlt`,
#'   representing the dates/times to test.  These need not be sorted.
#' @param TZ A single string naming an IANA time zone
#' (e.g. `America/New_York`). If `"local"`, it will extract local time
#' zone using \code{\link[base]{Sys.timezone}} The input must match
#' one entry in \code{\link[base]{OlsonNames}}.
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
#'
#' @return
#'   A logical vector of the same length as `DT`.
#'   \itemize{
#'     \item `TRUE` indicates the timestamp falls in DST
#'     \item `FALSE` indicates standard time.
#' }
#'
#'
#' @examples
#' # Standard use: New York in summer vs. winter
#' dates <- as.POSIXct(c("2021-06-15", "2021-12-15"),
#'                     tz = "America/New_York")
#' DST(dates, TZ = "America/New_York")
#'
#'
#' # Around the spring-forward transition
#' trans <- as.Date(c("2021-03-13", "2021-03-14", "2021-03-15"))
#' DST(trans, TZ = "America/New_York")
#'
#' # Using the local system time zone
#' DST(as.POSIXct("2023-07-01 12:00:00"), TZ = "local")
#'
#'
#' @keywords daylight dst
#' @export

DST <- function (Date, TZ = "local" , DT = NULL) {
  # Step 0. Input Validation and Parameter Extraction -----------
  if (all (c (is.null (Date), !is.null (DT)))) {
    lifecycle::deprecate_soft (
      when = "0.3.1",
      what = "DST(DT)",
      with = "DST(Date)"
    )
    Date <- DT
  }
  Date <- ValInput (Date, type = "Date")
  TZ   <- ValInput (TZ, type = "TZ")

  # Convert inputs to POSIXlt in the target TZ
  mD <- as.POSIXlt (Date, tz = TZ)

  Ldst <- mD$isdst != 0 # Base DST flag from POSIXlt


  # Compute day length deviations (hours gained/lost) via DST2GL()
  TCh <- DST2GL (Date = mD, TZ = TZ)


  # On transition Ds (|TCh| > 0), flip the DST flag
  Out <- ifelse (abs (TCh) > 0, !Ldst, Ldst)

  return (Out)
}
