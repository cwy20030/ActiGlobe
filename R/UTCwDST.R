# File ActiGlobe/R/UTCwDST.R
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
#' @title Determine if Daylight Saving Transitions may occur in an UTC Offset
#'
#' @description
#' Tests whether any IANA time zone associated with a given UTC offset
#' undergoes daylight saving time (DST) transitions. The function identifies
#' all zones matching the specified UTC offset and evaluates whether a time
#' change occurs between winter and summer timestamps.
#'
#' @details
#' For each value in `UTCs`, the function retrieves the corresponding IANA
#' time zones whose offset matches the specified UTC string or numeric hour.
#' It then compares the DST status of each zone on January 1 and July 15.
#' If at least one zone shows a shift in DST status between the two dates,
#' the function returns `TRUE` for that offset.
#'
#' @param UTCs
#'   A character vector or numeric vector representing UTC offsets.
#'   Accepted formats include `"UTC+08:00"`, `"UTC-05:00"`, or numeric values
#'   like `+8`, `-5`, etc. The function internally maps UTC strings to numeric
#'   offsets using `UTC2Num()`.
#'
#' @return
#'   A logical vector the same length as `UTCs`. Each entry is `TRUE` if at least
#'   one time zone at the specified offset undergoes a DST transition, `FALSE`
#'   otherwise.
#'
#' @examples
#'  \dontrun{
#' # Check for DST transitions in UTC+1 and UTC+8
#' UTCwDST(UTCs = c("UTC+01:00", "UTC+08:00"))
#'
#' # Use numeric offset directly
#' UTCwDST(UTCs = c(1, 8))
#'
#' # UTC−5 commonly includes DST zones (e.g., New York)
#' UTCwDST(UTCs = -5)
#'}
#' @seealso
#' \code{\link{DST}}, \code{\link{UTC2Num}}, \code{\link{OlsonNames}}
#'
#' @export
#'
UTCwDST <- function(UTCs) {

  OF = UTCs

  #### Convert UTC to Hour offset if not converted....
  if(any(grep("UTC|\\:", UTCs))) OF = UTC2Num(UTCs)

  #### Convert offset into the POSIX format
  aOF <- sprintf("%+03d00", OF)

  # Determine if DST exists using time offset on January 1st of 2021
  JAN1 <- as.POSIXct("2021-01-01", tz = "UTC")

  pTZs = GuessTZ(aOF = aOF)

  ## Check points for mispecified UTC offsets.
  if (any(lengths(pTZs) == 0L)) {
    TG = which(lengths(pTZs) == 0L)
    stop(sprintf("No matching found for following time zones: %s",
                 UTCs[TG]))
  }




  # Check DST status in mid‐winter vs. mid‐summer
  wDT <- as.POSIXct(JAN1, tz = "UTC") ### NO daylight saving time for the north hemispher but yes for the south
  sDT <- as.POSIXct("2021-07-15", tz = "UTC") ### Yes to daylight saving time for the north hemispher but NO for the south

  ## Check if any time zone may experience time change.
  Out <- sapply(pTZs, function(tzs) {
    wDST <- sapply(tzs, function(tz) as.POSIXlt(wDT, tz = tz)$isdst)
    sDST <- sapply(tzs, function(tz) as.POSIXlt(sDT, tz = tz)$isdst)

    any(wDST != sDST)
  })

  names(Out) = UTCs
  return(Out)
}
