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
#' all zones matching the specified UTC offset and evaluates whether time
#' change may occur between winter and summer timestamps.
#'
#'
#' @details
#' For each value in \code{UTCs}, the function retrieves the corresponding
#' IANA time zones whose offset matches the specified UTC string or numeric
#' hour. It then compares the DST status of each zone on January 1 and
#' July 15.
#'
#' **Important Note**
#' If at least one zone shows a shift in DST status between the two dates,
#' the function returns `TRUE` for that offset.
#'
#' To accommodate cases where mixed practices of DST within the same UTC
#' offset. Set Detail to `TRUE` to return a list with detailed information
#' about DST transitions for each time zone.
#'
#'
#' @importFrom stats setNames
#' @importFrom lifecycle deprecate_soft badge
#'
#'
#' @param UTCs A character vector or numeric vector representing UTC offsets.
#' Accepted formats include "UTC+08:00", "UTC-05:00", or numeric values
#' like "+8", "-5", etc. The function internally maps UTC strings to numeric
#' offsets using \code{\link{UTC2Num}}.
#' @param Date A POSIXct date used as the reference point for offset
#'   comparison. Defaults to January 1, 2021 UTC if `NULL`.
#' @param TZ A vector of time zone to prioritize. If `"local"`, the system
#'   time zone (`Sys.timezone ()`) is used. If `NULL`, no initial preference
#'   is applied.
#' @param fork Logical, if TRUE, it will use parallel processing to speed up
#' the computation. Default is FALSE.
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
#' @param Detail Logical, if TRUE, it will return a list with detailed
#' information about DST transitions for each time zone. Default is FALSE,
#' which returns only the summary of whether any DST transition occurs for
#' the respective UTC offset.
#'
#'
#' @return
#' \itemize{
#'  \item If \code{Detail = FALSE} (default): A logical vector the same
#'  length as \code{UTCs}. Each entry is `TRUE` if at least one time zone
#'  at the specified offset undergoes a DST transition, `FALSE` otherwise.
#'
#'  \item If \code{Detail = TRUE}: A list containing:
#'   \itemize{
#'    \item \code{any_dst}: A logical vector indicating whether any DST
#'    transition occurs for each UTC offset.
#'    \item \code{mixed_dst}: A logical vector indicating whether there are
#'    mixed DST practices within the same UTC offset (i.e., some zones shift
#'    while others do not).
#'    \item \code{by_tz}: A list of logical vectors for each UTC offset,
#'    showing the DST transition status for each associated time zone.
#'   }
#' }
#'
#'
#' @seealso
#' \code{\link{DST}} \code{\link{UTC2Num}} \code{\link{GuessTZ}}
#'
#'
#' @examples
#' # Check for DST transitions in UTC+1 and UTC+8
#' UTCwDST (UTCs = c ("UTC+01:00", "UTC+08:00"))
#'
#' # Use numeric offset directly
#' UTCwDST (UTCs = c (1, 8))
#'
#' # UTC-5 commonly includes DST zones (e.g., New York)
#' UTCwDST (UTCs = -5)
#'
#'
#' @export

UTCwDST <- function (UTCs, Date = NULL, TZ = NULL, fork = FALSE, DT = NULL,
                     Detail = FALSE) {
    # Step 0. Input Validation and Parameter Extraction -----------
    if (all (c (is.null (Date), !is.null (DT)))) {
        lifecycle::deprecate_soft (
            when = "0.3.1",
            what = "UTCwDST(DT)",
            with = "UTCwDST(Date)"
        )
        Date <- DT
    }

    ### Extract Essential
    OF <- UTCs

    ### Call mIANA
    sIANA <- iIANA # Time zone database
    sTZ   <- sIANA$TZ_IANA [sIANA$isDST == 0L]
    Soff  <- sIANA$Offset [sIANA$isDST]

    #### Convert UTC to Hour offset if not converted....
    if (any (grep ("UTC|\\:", UTCs))) OF <- UTC2Num (UTCs)

    #### Convert offset into the POSIX format
    aOF <- format_offset (x = OF)

    # Determine if DST exists using time offset on January 1st of 2021
    ## Default determine if DST exists using time offset on
    ## January 1st of 2021
    if (is.null (Date))
        Date   <- "2021-01-01"
    JAN1 <- as.Date (Date)

    # Check DST status in mid‐winter vs. mid‐summer ----------
    wDT <- as.POSIXct (JAN1, tz = "UTC")
    if (!length (wDT) == length (aOF))
        wDT <- rep (wDT, length (aOF))
    wDT <- setNames (wDT, aOF)

    sDT <- as.POSIXct ("2021-07-15", tz = "UTC")
    if (!length (sDT) == length (aOF))
        sDT <- rep (sDT, length (aOF))
    sDT <- setNames (sDT, aOF)

    ### GuessTZ
    if (all (!is.null (TZ))) {
        pTZs <- GuessTZ (aOF  = aOF,
                         DT   = Date,
                         All  = FALSE,
                         iTZ  = TZ,
                         fork = fork)
    } else {
        pTZs <- GuessTZ (aOF  = aOF,
                         DT   = Date,
                         All  = TRUE,
                         fork = fork)
    }

    # Step 1. Check for mis-specified UTC offsets ------------
    if (any (lengths (pTZs) == 0L)) {
        TG <- which (lengths (pTZs) == 0L)
        message (sprintf (
            "No matching found for following time zones: %s",
            UTCs [TG], " using OlsonNames. Trying mIANA..."
        ))

        pTZs [TG] <- sTZ [Soff %in% UTCs [TG]]
    }

    # Convert to list when there is only one aOF
    if (! inherits (pTZs, "list"))
        pTZs <- setNames (list (pTZs), aOF)

    # Step 2. Evaluate DST transitions for each UTC offset ------------
    ## Per-zone DST change flags (TRUE if that zone shifts between dates)
    tz_changes <- lapply (aOF, function (x) {
        wDST <- vapply (pTZs [[x]], function (tz)
            as.POSIXlt (wDT [[x]], tz = tz)$isdst,
            integer (1)
        )
        sDST <- vapply (pTZs [[x]], function (tz)
            as.POSIXlt (sDT [[x]], tz = tz)$isdst,
            integer (1)
        )
        setNames (wDST != sDST, pTZs [[x]])
    })
    names (tz_changes) <- aOF

    ## Aggregate per-offset summaries
    any_dst   <- vapply (tz_changes, any, logical (1))
    mixed_dst <- vapply (tz_changes, function (x)
        any (x) && any (!x),
        logical (1))

    # Step 3. Format Output ------------
    if (length (any_dst) == length (UTCs))
        names (any_dst) <- UTCs

    if (Detail) {
        return (list (
            any_dst   = any_dst,
            mixed_dst = mixed_dst,
            by_tz     = tz_changes
        ))
    }

    ## Preserve original return type but expose mixed info
    attr (any_dst, "mixed_dst") <- mixed_dst
    attr (any_dst, "by_tz") <- tz_changes

    return (any_dst)
}

#' @title Helper function to format offset in ±HHMM
#'
#' @param x Numeric vector representing hour offsets (e.g., -5.5, +8)
#'
#' @noRd
format_offset <- function (x) {
    # Separate hours and minutes
    hours <- trunc (x)
    minutes <- round ((x - hours) * 60)

    # Handle cases like 2.75 → 2h 45m
    sprintf ("%+03d%02d", hours, minutes)
}
