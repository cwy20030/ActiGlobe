# File ActiGlobe/R/DST.R
#
# Copyright (C) 2025  C. William Yao, PhD
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Daylight Saving Time
#'
#' @description
#' Determine if Dates Fall in Daylight Saving Time for the Given Time Zone(s)
#'
#' @details
#' For each element of `DT`, returns `TRUE` if that date-time is in
#' daylight saving time (DST) under the specified time zone.  The
#' function handles transitions by detecting the “gain” or “loss” of
#' hours via `DST2GL()` and flipping the DST flag on the transition day.
#'
#' @param DT
#'   A vector of class `Date`, `POSIXct`, or `POSIXlt`, representing
#'   the dates/times to test.  These need not be sorted.
#'
#' @param TZ
#'   A single string naming an IANA time zone (e.g. `"America/New_York"`).
#'   If `"local"`, it will extract local time zone using `Sys.timezone()`.  Must match one entry in
#'   `OlsonNames()`.
#'
#' @return
#'   A logical vector of the same length as `DT`.  `TRUE` indicates
#'   the timestamp falls in DST; `FALSE` indicates standard time.
#'
#' @examples
#' # Standard use: New York in summer vs. winter
#' dates <- as.POSIXct(c("2021-06-15", "2021-12-15"), tz = "America/New_York")
#' DST(dates, TZ = "America/New_York")
#'
#' # Around the spring-forward transition
#' trans <- as.Date(c("2021-03-13", "2021-03-14", "2021-03-15"))
#' DST(trans, TZ = "America/New_York")
#'
#' # Using the local system time zone
#' DST(as.POSIXct("2023-07-01 12:00:00"), TZ = "local")
#'
#' @export

DST <- function(DT, TZ = "local") {
  # Resolve "local" to the system time zone
  TZ <- ifelse(identical(TZ, "local"), Sys.timezone(),TZ)

  # Validate the time zone
  valid_zones <- OlsonNames()
  if (!TZ %in% valid_zones) {
    stop(sprintf(
      "The provided time zone \"%s\" is not recognized.\n",
      TZ
    ), "Please check the spelling or consult the IANA time zone table (ActiGlobe::IANA).")
  }

  # Convert inputs to POSIXlt in the target TZ
  mD <- as.POSIXlt(DT, tz = TZ)

  # Compute day length deviations (hours gained/lost) via DST2GL()
  TCh <- DST2GL(DT = mD)

  # Base DST flag from POSIXlt
  Ldst <- mD$isdst != 0

  # On transition days (|TCh| > 0), flip the DST flag
  Out <- ifelse(abs(TCh) > 0, !Ldst, Ldst)

      return(Out)
}
