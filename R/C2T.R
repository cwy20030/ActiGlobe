#  File ActiGlobe/R/C2T.R
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
#' @title Convert Time Values to Numeric (Decimal Hours)
#'
#' @description
#' Convert character or factor representations of time into numeric values
#' expressed in decimal hours. If the input is already numeric, it is coerced
#' directly. If the input is a time string, it is parsed into hours, minutes,
#' and seconds, then converted to decimal hours relative to the first entry.
#'
#' @param Time A vector of time values. Can be numeric, character, or factor.
#'   Examples include `"13:45:00"` or `"01:30:15"`. If numeric, values are
#'   returned as-is (after coercion). If character/factor, values are parsed
#'   using \code{\link{TimeFormat}}.
#' @param Discrete Logical scaler; if TRUE, each input in `Time` is converted
#'   individually without subtraction for the prior time.
#'
#' @returns
#' A numeric vector of time values expressed in decimal hours. If the input
#' is character/factor, the values are converted relative to the first entry
#' by default (i.e., the first element is set to 0 and subsequent values are
#' offsets in hours). When set TRUE to discrete, raw value is returned. If
#' coercion fails, `NA` values are introduced and a warning is issued.
#'
#' @seealso \code{\link{TimeFormat}}
#'
#' @examples
#'
#' # Character input
#' times <- c ("01:00:00", "02:30:00", "03:15:00")
#' C2T (times, Discrete = TRUE)
#'
#' @noRd

C2T <- function (Time, Discrete = FALSE) {
    x <- suppressWarnings (as.numeric (as.character (Time)))

    if (length (na.omit (x)) == 0) {
        hms <- as.POSIXct (Time, format = TimeFormat (Time))

        decimal_hours <- as.numeric (format (hms, "%H")) +
            as.numeric (format (hms, "%M")) / 60 +
            as.numeric (format (hms, "%S")) / 3600


        x <- as.numeric (decimal_hours)

    }

    if (!Discrete){

        x <- x - x [[1]] ## For duration
    }

    if (any (is.na (x))) warning (paste0 ("NAs introduced by coercion"))

    return (x)
}


### Potentially write a code to compute cosinor when both or neither the start or the end time falls are not the beginning/last time point of the recording.
