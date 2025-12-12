#  File ActiGlobe/R/Date2TotalT.R
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
#
#' @title Compute Total Time in Each Date (Per Day)
#'
#' @description
#' For an ordered vector of Dates or date-times, `Date2TotalT()` calculates the
#' interval from each element to the next (and for the last element, to the
#' following calendar day at midnight) and returns these durations in the
#' requested time unit.
#'
#' @param DT
#'   A vector of class `Date`, `POSIXct` or `POSIXlt`, sorted in ascending
#'   order. Each entry represents the start of an interval.  Internally, the
#'   function appends one extra day beyond the last entry so that the final
#'   interval covers a full 24 h until the next midnight. See \code{as.POSIXct}.
#'
#' @param TUnit
#'   Character string specifying the unit for the output durations.
#'   Must be one of `"hour"`, `"minute"`, or `"second"`.  Comparison is
#'   case-insensitive.  Default is `"hour"`, meaning the returned values
#'   are in decimal hours.
#'
#' @param TZ The time zone when the recording started. (default = "local",
#' which means user's local time zone)
#'
#' @return
#'   A numeric vector of the same length as `DT`.  Each element is the
#'   elapsed time between the corresponding entry in `DT` and the next
#'   boundary (either the next date-time in `Date`, or midnight following
#'   the last date), expressed in the units given by `TUnit`.
#'
#' @examples
#' \donttest{
#' # Two calendar days: returns c(24, 24) hours
#' Date2TotalT(as.Date(c("2021-01-01", "2021-01-02")), "hour")
#'
#'
#' # Working in minutes
#' Date2TotalT(as.POSIXct(c(
#'   "2021-06-10 08:00:00",
#'   "2021-06-10 14:30:00"
#' )), "minute")
#' }
#'
#' # In seconds (case-insensitive unit name)
#' Date2TotalT(as.Date("2022-12-31"), "SeCoNd")
#'
#' @export
Date2TotalT <- function(DT, TUnit = "hour", TZ = "local") {
  TZ <- ifelse(TZ == "local", Sys.timezone(), TZ)

  # Validate the time zone
  valid_zones <- OlsonNames()
  if (!TZ %in% valid_zones) {
    stop(sprintf(
      "The provided time zone \"%s\" is not recognized.\n",
      TZ
    ), "Please check the spelling or consult the IANA time zone table
    (ActiGlobe::IANA).")
  }

  ## Convert the displayed unit into a factor.
  TDivider <-
    UnitFactor(
      x = TUnit,
      method = "Time"
    )

  #### Compute the supposed data points for each day ---------------
  sTotalSec <-
    sapply(DT, function(D) {
      MxD <- as.character(as.POSIXct(paste(max(as.Date(D, tz = TZ)) + 1,
                                           " 00:00:00"), tz = TZ))
      # Vector 1 for the starting date
      iniDs <- as.character(as.POSIXct(paste(as.Date(D, tz = TZ),
                                             " 00:00:00"), tz = TZ))
      endDs <- MxD # Vector 2 for the next date

      as.numeric(as.POSIXct(endDs, tz = TZ)) -
        as.numeric(as.POSIXct(iniDs, tz = TZ)) # Supposed seconds for each day
    })
  Out <- sTotalSec / TDivider # Convert the output based on TUnit

  return(Out)
}
