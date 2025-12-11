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
#' @param Bdf A \code{\link{BriefSum}} object containing per-day metadata for the recording. Note, if jet lag occurred during the recording, please, update the metadata using \code{\link{TAdjust}} before passing to this function.
#' @param D The date travelling by plane.
#' @param U The UTC offset values <e.g., "UTC+09:30" or "UTC-07:00">
#'
#' @returns
#' A data frame containing the following columns:
#' \itemize{
#'   \item \code{Date} — The recording dates.
#'   \item \code{Recording_Period} — An integer index indicating the travel period
#'         (e.g., before/after flight segments).
#'   \item \code{UTC} — The updated UTC offset string for each date.
#'   \item \code{Hour_to_Adjust} — Numeric values representing the difference in hours
#'         between the old and new UTC offsets, used for time adjustment.
#' }
#'
#' @examples
#' data (FlyEast)
#'
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
#'     BriefSum (
#'         df = FlyEast,
#'         SR = 1 / 60,
#'         Start = "2017-10-24 13:45:00"
#'     )
#'
#' # Let's extract actigraphy data from a single day
#' Bdf <- BdfList$Bdf
#'
#' data (TLog)
#' R2P (Bdf = Bdf,
#'      D = TLog$date_Start,
#'      U = TLog$UTC_Offset)
#'
#' @noRd


R2P <- function (Bdf, D, U) {
    ## Extract Date info from summary
    DT <- Bdf$Date ## All Recording Dates
    DT <- DateFormat (DT)
    MinDate <- min (DT) ## First Date
    MaxDate <- max (DT) ## Last Date


    ## Check if UTC is in the Bdf
    if (!"UTC" %in% names (Bdf)) stop ("Bdf must be an object created by BriefSum.")


    ## Extract UTC offset
    if (any (grepl ("UTC", U))) U <- UTC2Num (U)


    #### Double check for date coherence ---------
    D <- DateFormat(D)


    # Process UTC and Time adjustment -------------

    Bdf$UTC.old <- Bdf$UTC [[1]]

    for (d in seq_along(D)) {
        if (d < length (D)) {
            Period <- as.Date (D [d]:(D [d + 1] - 1))

        } else {
            fD <- as.integer (which (DT == D [d]))
            eD <- as.integer (which (DT == MaxDate))

            nD <- (eD - fD) + 1

            idx <- seq (
                from = fD,
                to = eD,
                length.out = nD
            )

            Period <- as.Date (DT [idx])
        }

        Bdf$Recording_Period [DT %in% Period] <- d
        Bdf$UTC [DT %in% Period] <- Num2UTC (U [d])
    }

    ### Compute Changes in Hours -------------
    Bdf$Hour_to_Adjust <- UTC2Num (Bdf$UTC) - UTC2Num (Bdf$UTC.old)


    ## Update Recording Period
    if (!MinDate %in% D) {
        Bdf$Recording_Period <- ifelse (is.na (Bdf$Recording_Period), 1, Bdf$Recording_Period + 1)
    }


    # Output --------
    Out <- Bdf [c ("Date", "Recording_Period", "UTC", "Hour_to_Adjust")]
    return (Out)
}
