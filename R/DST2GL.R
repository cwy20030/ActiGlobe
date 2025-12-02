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
#' DST2GL computes, for each date-time entry, how many hours a day
#' deviates from the standard 24hours because of DST shifts.
#' Internally it calls \code{Date2TotalT()} in seconds, subtracts
#' the 86400s of a normal day, and converts the remainder to hours.
#'
#' @param DT
#'   A vector of class \code{Date}, \code{POSIXct}, or \code{POSIXlt},
#'   sorted in ascending order.  Each element marks the start of a day
#'   boundary.  To capture DST transitions, include at least one day
#'   before and after the expected shift. Note that the time zone should
#'   be specified in the DT. See \code{as.POSIXct}.
#'
#' @param TZ The time zone when the recording started. (default = "local", which means user's local time zone)
#' @return
#'   A numeric vector of the same length as \code{DT}.  Each
#'   value is the number of hours that day is longer
#'   (positive) or shorter (negative) than 24h.  A zero means no DST
#'   shift on that date.
#'
#' @examples
#' # Example around a typical spring-forward transition (e.g. US second Sunday in March)
#' dates <- as.Date (c ("2021-03-13", "2021-03-14", "2021-03-15"))
#'
#' # On 2021-03-14 clocks jumped forward: day is 23h so output = -1
#' DST2GL (dates)
#'
#' \donttest{
#' # Example around a fall-back transition (e.g. first Sunday in November)
#' dates <- as.Date (c ("2021-10-31", "2021-11-01", "2021-11-02"))
#'
#' # On 2021-11-07 clocks fall back: day is 25h so output = +1
#' DST2GL (dates)
#' }
#' @export


DST2GL <- function (DT, TZ = "local") {

    TZ <- ifelse (TZ == "local", Sys.timezone (), TZ)

    sFDPs <- Date2TotalT (DT = DT, TUnit = "second", TZ = TZ)

    Out <- (sFDPs - (86400)) / 3600 ### Compute total numbers of hours in difference

    return (Out)
}
