# File ActiGlobe/R/UTCwDST.R
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
#' @param UTCs A character vector or numeric vector representing UTC offsets.
#' Accepted formats include `"UTC+08:00"`, `"UTC-05:00"`, or numeric values
#' like `+8`, `-5`, etc. The function internally maps UTC strings to numeric
#' offsets using `UTC2Num()`.
#' @param DT A POSIXct date/time used as the reference point for offset
#'   comparison. Defaults to January 1, 2021 UTC if `NULL`.
#' @param fork Logical, if TRUE, it will use parallel processing to speed up
#' the computation. Default is FALSE.
#'
#' @return
#'  A logical vector the same length as `UTCs`. Each entry is `TRUE` if at
#'  least one time zone at the specified offset undergoes a DST transition,
#'  `FALSE` otherwise.
#'
#' @seealso
#' \code{\link{DST}} \code{\link{UTC2Num}} \code{\link{OlsonNames}}
#'
#' @examples
#' \dontrun{
#' # Check for DST transitions in UTC+1 and UTC+8
#' UTCwDST (UTCs = c ("UTC+01:00", "UTC+08:00"))
#'
#' # Use numeric offset directly
#' UTCwDST (UTCs = c (1, 8))
#'
#' # UTC-5 commonly includes DST zones (e.g., New York)
#' UTCwDST (UTCs = -5)
#' }
#'
#' @export

UTCwDST <- function (UTCs, DT = NULL, fork = FALSE) {
    # Extract Essential -------------------
    OF <- UTCs

    ### Call mIANA-------------------
    sIANA <- mIANA () # Time zone database
    iTZ <- sIANA$Timezone_IANA
    Soff <- sIANA$Standard_Offset


    #### Convert UTC to Hour offset if not converted....
    if (any (grep ("UTC|\\:", UTCs))) OF <- UTC2Num (UTCs)

    #### Convert offset into the POSIX format
    aOF <- format_offset (x = OF)

    # Determine if DST exists using time offset on January 1st of 2021
    if (is.null (DT))
        ## Default determine if DST exists using time offset on
        ## January 1st of 2021
        DT <- "2021-01-01"
        JAN1 <- as.POSIXct (DT, tz = "UTC")

    pTZs <- GuessTZ (aOF = aOF,
                     DT = DT,
                     All = TRUE,
                     fork = fork)

    ## Check points for mispecified UTC offsets ------------
    if (any (lengths (pTZs) == 0L)) {
        TG <- which (lengths (pTZs) == 0L)
        message (sprintf (
            "No matching found for following time zones: %s",
            UTCs [TG], " using OlsonNames.Try mIANA..."
        ))

        pTZs [TG] <- iTZ [Soff %in% UTCs [TG]]
    }

    # Check DST status in mid‐winter vs. mid‐summer ----------
    wDT <- as.POSIXct (JAN1, tz = "UTC")
    ### NO daylight saving time for the north hemisphere but yes for the south

    sDT <- as.POSIXct ("2021-07-15", tz = "UTC")
    ### Yes to daylight saving time for the north hemisphere
    ### but NO for the south

    ## Check if any time zone may experience time change.
    Out <- vapply (pTZs, function (tzs) {
        wDST <- vapply (tzs, function (tz) as.POSIXlt (wDT, tz = tz)$isdst,
                        integer (1))
        sDST <- vapply (tzs, function (tz) as.POSIXlt (sDT, tz = tz)$isdst,
                        integer (1))
        any (wDST != sDST)
    }, logical (1))

    if (length (Out) == length (UTCs)) {
        names (Out) <- UTCs
    }

    return (Out)
}


#' @title Helper function to format offset in ±HHMM
#'
#' @param x Numeric vector representing hour offsets (e.g., -5.5, +8)
#'
#' @noRd
#'
format_offset <- function (x) {
    # Separate hours and minutes
    hours <- trunc (x)
    minutes <- round ((x - hours) * 60)

    # Handle cases like 2.75 → 2h 45m
    sprintf ("%+03d%02d", hours, minutes)
}
