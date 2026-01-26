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
#' An exhaustive process was
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
#'   time zone (`Sys.timezone ()`) is used. If `NULL`, no initial preference
#'   is applied.
#' @param All Logical. If `TRUE` (default), return all matching time zones
#'   for each offset. If `FALSE`, return only the first match per offset.
#' @param fork Logical. If `TRUE`, use parallel processing via the
#'   \pkg{parallel} package to compute offsets across all Olson time zones.
#'   Defaults to `FALSE` (sequential).
#'
#' @return One of the following, depending on the inputs:
#' 
#'  \itemize{
#'   \item If \code{length (aOF) == 1} and \code{All = TRUE}:
#'     \itemize{
#'       \item When multiple matches are found: a character matrix with one
#'       column. The column name is the offset and there is one row per
#'       matching time zone.
#'       \item When a single match is found: a named character vector of
#'       length 1, with the name set to the offset.
#'     }
#' 
#'   \item If \code{length (aOF) > 1} and \code{All = TRUE}: a named list
#'   (names are the offsets in \code{aOF}). Each element is a character
#'   vector of matching time zones. Elements may be \code{character (0)} if
#'   no matches are found.
#' 
#'   \item If \code{All = FALSE}:
#'     \itemize{
#'       \item When \code{length (aOF) == 1}: a single character scalar (the
#'       first match).
#'       \item When \code{length (aOF) > 1}: a named character vector (one
#'       element per offset) giving the first match for each offset.
#'     }
#' }
#' In all cases the returned values represent candidate Olson/IANA time
#' zone identifiers corresponding to the supplied offset(s).
#'
#' @examples
#'
#' # Guess time zones for UTC-5 and UTC+1 offsets
#' GuessTZ (c ("-0500", "+0100"))
#'
#' # Restrict to first match per offset
#' GuessTZ (c ("-0500", "+0100"), All = FALSE)
#'
#' # Prioritize local system time zone
#' GuessTZ (c ("-0500"), iTZ = "local")
#'
#' # Use parallel processing
#' GuessTZ (c ("+0000"), fork = TRUE)
#'
#' @noRd

GuessTZ <- function (aOF, DT = NULL, iTZ = NULL, All = TRUE, fork = FALSE) {
    # Check Point ----------------------
    ## Time Zone
    ## Establish initial time zone

    ### Validate the time zone using OlsonNames()
    ### Extract all known time zones
    oTZs <- OlsonNames ()

    if (is.null (iTZ)) {
        TZ1 <- NULL
    } else if (iTZ == "local") {
       TZ1 <- Sys.timezone ()
    } else if (!iTZ %in% oTZs) {
        stop (sprintf (
            "The provided time zone \"%s\" is not recognized.\n",
            iTZ
        ), "Please check the spelling or consult the IANA time zone table
    (ActiGlobe::IANA).")
    } else {
        TZ1 <- iTZ
    }

    ## Process DT
    if (is.null (DT)) {
        ## Determine if DST exists using time offset on January 1st of 2021
        DT <- as.POSIXct ("2021-01-01", tz = "UTC")
    }

    if (!length (DT) == 1) DT <- DT [[1]]


    # Date-based time zone identification -----------------------
    ## Step 0. Extract time offsets for all time zones ------------------
    ### Parallel version
    if (fork) {
        ### Step 1: Create a cluster
        NCore <- parallel::detectCores ()
        ### leave a couple cores free
        cl <- parallel::makeCluster (max (1, NCore - 2))

        ### Step 2: Export variables
        parallel::clusterExport (cl, varlist = c ("DT"), envir = environment ())

        ### Step 3: Run parallelized task
        Toffs <- parallel::parLapply (cl, oTZs, function (tz) {
            format (as.POSIXct (DT, tz = tz), "%z")
        })

        ### Step 4: Clean up
        parallel::stopCluster (cl)

        Toffs <- unlist (Toffs)

    } else { ### Sequential version ---------------

        Toffs <- vapply (
            oTZs, function (tz) {
                format (as.POSIXct (DT, tz = tz), "%z")
            },
            character (1)
        )
    }


    ## Step 1 Guess all possible TZ indicators -----------
    pTZs <-  if (length (aOF) == 1) {
        res <- oTZs [Toffs %in% aOF]
        if (length (res) > 1) {

            matrix (res, ncol = 1, dimnames = list (NULL, aOF))

        } else {
            setNames (res, aOF)
        }

    } else {
        setNames (
        lapply (aOF, function (x) {
            oTZs [Toffs %in% x]
        }),
        aOF
        )
    }

    ## Step 2 Check if the initial time zone is included
    if (!is.null (TZ1)) {
        if (length (aOF) == 1) {
             if (TZ1 %in% pTZs) {
            pTZs <-
                matrix (TZ1,
                        nrow = 1,
                        ncol = 1,
                        dimnames = dimnames (pTZs))
             }

        } else {
            pTZs <- setNames (
                lapply( pTZs, function (i) {
                if (TZ1 %in% i) TZ1 else i
                }),
                aOF
            )
        }
    }

    ## Step 3 Keep only the first one if the All is set to FALSE
    if (!All) {
        if (length (aOF) > 1) {
            pTZs <- vapply (pTZs, function (x) x [[1]], character (1))
        } else {
            pTZs <- pTZs [[1]]
        }
    }

    return (pTZs)
}
