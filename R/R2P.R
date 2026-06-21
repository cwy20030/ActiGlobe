#  File ActiGlobe/R/R2P.R
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
#
#' @title Convert Travelling Log to Parameters for TAdjust
#'
#' @param Bdf A \code{\link{BriefSum}} object containing per-day metadata for
#' the recording. Note, if jet lag occurred during the recording, please,
#'  update the metadata using \code{\link{TAdjust}} before passing to this
#'  function.
#' @param Ds The date travelling by plane.
#' @param U The UTC offset values <e.g., "UTC+09:30" or "UTC-07:00">
#' @param has.DST Logical scalar indicating local practice of daylight saving
#' time.
#'
#' @returns
#' A data frame containing the following columns:
#' \itemize{
#'   \item \code{Date} — The recording dates.
#'   \item \code{Prd} — An integer index indicating the travel
#'         period (e.g., before/after flight segments).
#'   \item \code{UTC} — The updated UTC offset string for each date.
#'   \item \code{H2J} — Numeric values representing the difference
#'         in hours between the old and new UTC offsets, used for time
#'         adjustment.
#'   \item \code{hsDST} — A logical vector indicating whether daylight saving
#'         time is observed for each date.
#' }
#'
#' @examples
#' data (FlyEast)
#'
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
#'     BriefSum (
#'         data = FlyEast,
#'         SR = 1 / 60,
#'         Start = "2017-10-24 13:45:00"
#'     )
#'
#' # Let's extract actigraphy data from a single day
#' Bdf <- BdfList$Bdf
#' R2P (
#'     Bdf     = Bdf,
#'     Ds      = TLog$Date_Start,
#'     U       = TLog$UTC_Offset
#'     has.DST = TLog$Country_with_Daylight_Saving
#' )
#'
#' @noRd


R2P <- function (Bdf, Ds, U, has.DST = NULL) {
    ## Extract Date info from summary
    DT <- Bdf$Date ## All Recording Dates
    DT <- DateFormat (DT)
    MinDate <- min (DT) ## First Date
    MaxDate <- max (DT) ## Last Date

    ## Check if UTC is in the Bdf
    if (!"UTC" %in% names (Bdf)) {
        stop ("Bdf must be an object created by BriefSum.")
    }

    ## Extract UTC offset
    if (any (grepl ("UTC", U))) U <- UTC2Num (U)


    #### Double check for date coherence ---------
    Ds <- suppressWarnings (DateFormat (Ds))


    ## Identify DST based on the initial summary----------------
    Bdf$has_DST <- UTCwDST (UTCs = Bdf$UTC [1],
                            TZ   = Bdf$TZ_code [1])


    ## Guess if the UTC may experience Time Change due to daylight saving
    ## calender
    if (all (is.null (has.DST)) || all (is.na (has.DST)) ||
        all (has.DST == ""))
        has.DST <- UTCwDST (UTCs = U)


    # Process UTC and Time adjustment -------------

    Bdf$UTC.old <- Bdf$UTC [[1]]

    for (i in seq_len (length (Ds))) {
        if (i < length (Ds)) {
            Period <- as.Date (Ds [i]:(Ds [i + 1] - 1))
        } else {
            fD <- as.integer (which (DT == Ds [i]))
            eD <- as.integer (which (DT == MaxDate))

            nD <- (eD - fD) + 1

            idx <- seq (
                from = fD,
                to = eD,
                length.out = nD
            )

            Period <- as.Date (DT [idx])
        }

        Bdf$Recording_Period [DT %in% Period] <- i
        Bdf$UTC [DT %in% Period]              <- Num2UTC (U [i])
        Bdf$has_DST [DT %in% Period]          <- has.DST [i]
    }

    ### Compute Changes in Hours -------------
    Bdf$Hour_to_Adjust <- UTC2Num (Bdf$UTC) - UTC2Num (Bdf$UTC.old)


    ## Update Recording Period
    if (!MinDate %in% Ds) {
        Bdf$Recording_Period <- ifelse (is.na (Bdf$Recording_Period), 1,
            Bdf$Recording_Period + 1
        )
    }


    # Output --------
    Out <- setNames (
        Bdf [c ("Date", "Recording_Period", "UTC", "Hour_to_Adjust",
                   "has_DST")],
        c ("Date", "Prd", "UTC", "H2J", "hsDST")
    )

    return (Out)
}
