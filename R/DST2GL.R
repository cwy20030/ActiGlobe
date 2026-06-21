#  File ActiGlobe/R/DST2GL.R
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
#' @title Compute Time Gain/Loss due to Daylight Saving Time
#'
#' @description
#' DST2GL computes, for each date or date-time entry, how many hours a day
#' deviates from the standard 24hours because of DST shifts.
#' Internally it calls \code{\link{Date2TotalT}} in seconds, subtracts
#' the 86400s of a normal day, and converts the remainder to hours. Note,
#' while **DST** is part of the function name, DST2GL intends to capture
#' also non-standard time shift.
#'
#' @details
#' Non-uniform Shift in Offset Values:
#' While around most daylight saving time (DST) transitions have
#' offset by an hour since 1980, various exceptions exists (e.g.,)
#'
#'
#' Output Signs and Coordinated Universal Time (UTC) Offsets:
#' During transitioning from standard time to DST, the clock
#' springs forward by a set number of minutes. This results in
#' a shorter day (e.g., 23h). In this case, the output of DST2GL will
#' be negative (e.g., -1) and vice versa.
#'
#'
#' @importFrom lifecycle badge
#'
#' @param Date A vector of class \code{Date}, \code{POSIXct}, or \code{POSIXlt},
#'   sorted in ascending order.  Each element marks the start of a day
#'   boundary.  To capture DST transitions, include at least one day
#'   before and after the expected shift. Note that the time zone should
#'   be specified in the DT. See \code{as.POSIXct}.
#' @param TZ The time zone when the recording started. (default = "local",
#'  which means user's local time zone)
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
#'
#' @return
#'   A numeric vector of the same length as \code{Date}. Each
#'   value is the number of hours that day is longer (positive)
#'   or shorter (negative) than 24h. A zero means no DST shift
#'   on that date.
#'
#' @seealso \code{\link{Date2TotalT}} \code{\link{DST}} \code{\link{TZwDST}}
#'
#' @examples
#' # Example of standard time to daylight saving time transition
#' # On 2021-03-14 clocks jumped forward: day is 23h so output = -1
#' DST2GL(Date = "2021-03-14", TZ = "America/New_York")
#'
#' # Example around a fall-back transition (e.g. first Sunday in November)
#' dates <- as.Date("2021-11-07")
#'
#' # On 2021-11-07 clocks fall back: day is 25h so output = +1
#' DST2GL(dates)
#'
#' # Non-standard time shift
#' DST2GL(Date= "3/29/1992", TZ = "America/Guyana")
#'
#'
#' # Multiple Dates
#' sapply(c("2021-03-13", "2021-03-14", "2021-03-15"),
#'        DST2GL)
#'
#' @keywords dst
#' @export


DST2GL <- function (Date,
                    TZ = "local",
                    DT = NULL) {
    # Step 0. Input Validation and Parameter Extraction -----------
    if (all (c (is.null (Date), !is.null (DT)))) {
        lifecycle::deprecate_soft (
            when = "0.3.1",
            what = "DST2GL(DT)",
            with = "DST2GL(Date)"
        )
        Date <- DT
    }
    Date <- ValInput (Date, type = "Date")
    TZ   <- ValInput (TZ, type = "TZ")

    # Step 1. Compute total seconds in each day (including DST shifts) ----
    sFDPs <- Date2TotalT (Date = Date, TUnit = "second", TZ = TZ)
    sFDPs <- as.numeric (sFDPs)

    ### Compute total numbers of hours in difference
    Out <- (sFDPs - (86400)) / 3600

    names (Out) <- Date
    return (Out)
}
