# File ActiGlobe/R/GuessTZ.R
#
# Copyright (C) 2025  C. William Yao, PhD
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
#' @title Guess Time Zones from Offsets
#'
#' @description
#' The `GuessTZ()` function attempts to infer possible time zones based on
#' observed time offsets (`aOF`) at a given reference date/time. It compares
#' offsets against all known Olson time zones, optionally prioritizing an
#' initial time zone (`iTZ`) and allowing parallel computation for speed.
#'
#' @details
#' - Uses `OlsonNames()` to retrieve all known time zones.
#' - Computes offsets for each zone at the reference date/time (`DT`).
#' - Matches observed offsets (`aOF`) against computed offsets.
#' - If `iTZ` is provided, it is prioritized when present in the matches.
#' - Parallel computation (`fork = TRUE`) uses all but two cores by default.
#'
#' @import parallel
#'
#' @param aOF A character vector of observed time offsets (e.g., `"+0100"`,
#'   `"-0500"`). Each element is matched against known time zone offsets.
#' @param DT A POSIXct date/time used as the reference point for offset
#'   comparison. Defaults to January 1, 2021 UTC if `NULL`.
#' @param iTZ An initial time zone to prioritize. If `"local"`, the system
#'   time zone (`Sys.timezone()`) is used. If `NULL`, no initial preference
#'   is applied.
#' @param All Logical. If `TRUE` (default), return all matching time zones
#'   for each offset. If `FALSE`, return only the first match per offset.
#' @param fork Logical. If `TRUE`, use parallel processing via the
#'   \pkg{parallel} package to compute offsets across all Olson time zones.
#'   Defaults to `FALSE` (sequential).
#'
#' @return A character vector or list of character vectors containing
#'   candidate time zones corresponding to each offset in `aOF`. If `All = FALSE`,
#'   only the first match is returned per offset.
#'
#' @examples
#'
#' # Guess time zones for UTC-5 and UTC+1 offsets
#' GuessTZ(c("-0500", "+0100"))
#'
#' # Restrict to first match per offset
#' GuessTZ(c("-0500", "+0100"), All = FALSE)
#'
#' # Prioritize local system time zone
#' GuessTZ(c("-0500"), iTZ = "local")
#'
#' # Use parallel processing
#' GuessTZ(c("+0000"), fork = TRUE)
#'
#'
#' @noRd

GuessTZ <- function (aOF, DT = NULL, iTZ = NULL, All = TRUE, fork = FALSE) {
    # Establish initial time zone ----------------
    TZ1 <- ifelse (iTZ == "local", Sys.timezone (), iTZ)

    if (is.null (iTZ)) TZ1 <- NULL


    # Extract all known time zones ----------------
    oTZs <- OlsonNames ()

    ## Process DT
    if (is.null (DT)) {
        # Determine if DST exists using time offset on January 1st of 2021
        DT <- as.POSIXct ("2021-01-01", tz = "UTC")
    }

    if (!length (DT) == 1) DT <- DT [[1]]


    # Extract time offsets for all time zones ----------------
    if (fork) {
        # Step 1: Create a cluster
        NCore <- parallel::detectCores ()
        cl <- parallel::makeCluster (max (1, NCore - 2)) # leave a couple cores free

        # Step 2: Export variables
        parallel::clusterExport (cl, varlist = c ("DT"), envir = environment ())

        # Step 3: Run parallelized task
        Toffs <- parallel::parLapply (cl, oTZs, function (tz) {
            format (as.POSIXct (DT, tz = tz), "%z")
        })

        # Step 4: Clean up
        parallel::stopCluster (cl)

        Toffs <- unlist (Toffs)
    } else {
        ## Sequential version
        Toffs <- vapply (
            oTZs,
            function (tz) format (as.POSIXct (DT, tz = tz), "%z"),
            character (1)
        )
    }


    #### Step 1 Guess all possible TZ indicators -----------
    pTZs <- sapply (
        aOF,
        function (x) oTZs [Toffs %in% x]
    )

    #### Step 2 Check if the initial time zone is included
    if (!is.null (TZ1)) {
        if (length (aOF) == 1) {
            pTZs <- ifelse (TZ1 %in% pTZs, TZ1, pTZs)
        } else {
            pTZs <- sapply (
                pTZs,
                function (x) ifelse (TZ1 %in% x, TZ1, x)
            )
        }
    }


    #### Step 3 Keep only the first one if the All is set to FALSE
    if (!All) {
        if (length (aOF) > 1) {
            pTZs <- sapply (
                pTZs,
                function (x) x [[1]]
            )
        } else {
            pTZs <- pTZs [[1]]
        }
    }


    return (pTZs)
}
