#  File ActiGlobe/R/Num2UTC.R
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
#
#' @title Convert Numbers to Standard UTC Offset
#'
#' @description
#' Convert numeric values representing UTC offsets (in hours, with
#' optional fractional parts for minutes) into standardized character
#' strings of the form `"UTC+HH:MM"` or `"UTC-HH:MM"`. This is useful
#' for translating numeric time zone offsets into human-readable UTC
#' offset notation.
#'
#' @param x UTC Offsets <e.g., "UTC+09:30" or "UTC-07:00">
#'
#' @return A character vector of standard UTC offsets
#'
#' @examples
#' # Convert UTC to numeric values
#' x <- c (9.5, -7)
#'
#' x1 <- Num2UTC (x)
#'
#' print (x1)
#'
#' @seealso \code{\link{UTC2Num}}
#' @export

Num2UTC <- function (x) {
    # Check Point -------------------------
    if (any (!is.numeric (x))) x <- as.numeric (as.character (x))
    if (any (abs (x) > 14) || any (is.na (x))) {
        stop ("UTC offsets must be between -14 and +14 hours.")
    }

    ## Get the hours
    A <- ifelse (x < 0, ceiling (x), floor (x)) ### Hour unit
    B <- (abs (x) - abs (A)) * 60 ### Minute unit


    mp <- ifelse (x < 0, "-", "+") ### Check positive or negative

    C <- ifelse (abs (A) < 10, paste0 (mp, "0", abs (A)), paste0 (mp, A))
    D <- ifelse (B == 0, "00", B)


    Out <- paste0 ("UTC", C, ":", D)


    return (Out)
}
