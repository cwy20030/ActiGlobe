#  File ActiGlobe/R/UTC2Num.R
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
#' @title Convert Standard UTC Offset to Numbers
#'
#' @param x UTC Offsets <e.g., "UTC+09:30" or "UTC-07:00">
#'
#' @return Numeric values of UTC offsets
#'
#' @examples
#' # Convert UTC to numeric values
#' x <- c ("UTC+09:30", "UTC-07:00")
#'
#' x1 <- UTC2Num (x)
#'
#' print (x)
#'
#' @keywords UTC num offset
#' @export

UTC2Num <- function (x) {
    # Check Point -------------------------
    x <- toupper (x)
    if (any (!grepl ("UTC", x)) || any (is.na (x)) || any (is.numeric (x))) {
        stop ("Input UTC offsets must be in the format of 'UTC+HH:MM' or
         'UTC-HH:MM'.")
    }


    # Step 1: Remove "UTC" and white space -----------------------------
    x <- gsub ("UTC", "", x)
    x <- trimws (x) # Remove white space

    # Step 2: Validate the format -------------------------
    mp <- vapply (x, function (i) {
        ifelse (grepl ("-", i), -1, 1)
    }, numeric (1))
    a <- gsub ("\\+|\\-", "", x) #### Remove plus minus sign

    b <- as.numeric (gsub (":.*", "", a))
    c <- as.numeric (gsub (".*:", "", a)) / 60

    # Step 3: Combine to get numeric values -------------------------
    x <- mp * (b + c)

    return (x)
}
