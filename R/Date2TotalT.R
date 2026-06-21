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
#' @title Compute Total Time in Each Date (Per Day)
#'
#' @description
#' For an ordered vector of Dates or date-times, `Date2TotalT()` calculates the
#' interval from each element to the next (and for the last element, to the
#' following calendar day at midnight) and returns these durations in the
#' requested time unit.
#'
#' @importFrom lifecycle badge
#'
#' @param Date A vector of class \code{Date}, \code{POSIXct}, or
#'   \code{POSIXlt}, sorted in ascending order. Each entry represents the
#'   start of an interval. Internally, the function appends one extra day
#'   beyond the last entry so that the final interval covers a full day
#'   until the next midnight. See \code{as.POSIXct}.
#' @param TUnit Character string specifying the unit for the output duration.
#'   Must be one of "hour", "minute", or "second".  Comparison is case-
#'   insensitive.  Default is "hour", meaning the returned values
#'   are in decimal hours.
#' @param TZ The time zone when the recording started. (default = "local",
#' which means user's local time zone)
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
#'
#' @return
#'   A numeric vector of the same length as \code{DT}.  Each element is the
#'   elapsed time between the corresponding entry in \code{DT} and the next
#'   boundary (either the next date-time in \code{Date}, or midnight following
#'   the last date), expressed in the units given by \code{TUnit}.
#'
#' @examples
#' # Two calendar days: returns c(24, 24) hours
#' Date2TotalT(Date  = as.Date(c("2021-01-01", "2021-01-02")),
#'             TUnit = "hour")
#'
#'
#' # Working in minutes
#' Date2TotalT(Date  = as.POSIXct(c("2021-06-10 08:00:00",
#'                                  "2021-06-10 14:30:00")),
#'             TUnit = "minute")
#'
#'
#' # In seconds (case-insensitive unit name)
#' Date2TotalT(Date  = as.Date("2022-12-31"),
#'             TUnit = "SeCoNd")
#'
#' @keywords time duration
#' @export

Date2TotalT <- function (Date,
                         TUnit = "hour",
                         TZ    = "local",
                         DT     = NULL) {
    # Step 0. Input Validation and Parameter Extraction -----------
    if (all (c (is.null (Date), !is.null (DT)))) {
        lifecycle::deprecate_soft (
            when = "0.3.1",
            what = "Date2TotalT(DT)",
            with = "Date2TotalT(Date)"
        )
        Date <- DT
    }
    Date <- ValInput (Date, type = "Date")
    TZ   <- ValInput (TZ, type = "TZ")

    ## Convert the displayed unit into a factor.
    TDivider <-
        UnitFactor (
            x = TUnit,
            method = "Time"
        )

    # Step 1. Compute the supposed data points for each day ---------------
    sTotalSec <-
        vapply (Date, function (D) {
            MxD <- as.character (as.POSIXct (paste (
                max (as.Date (D, tz = TZ)) + 1,
                " 00:00:00"
            ), tz = TZ))
            # Vector 1 for the starting date
            iniDs <- as.character (as.POSIXct (paste (
                as.Date (D, tz = TZ),
                " 00:00:00"
            ), tz = TZ))
            endDs <- MxD # Vector 2 for the next date

            # Supposed seconds for each day
            as.numeric (as.POSIXct (endDs, tz = TZ)) -
                as.numeric (as.POSIXct (iniDs, tz = TZ))
        }, numeric (1))

    # Step 2. Convert the output into the requested time unit ---------------
    Out <- sTotalSec / TDivider

    # Step 3. Return the output ---------------
    return (Out)
}
