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
#' @title Convert Coordinated Universal Time (UTC) Offset to Numeric Values
#'
#' @param x Character string of UTC offsets, e.g., "UTC+09:30" or "UTC-07:00".
#'
#' @return Numeric values of UTC offsets
#'
#' @seealso \code{\link{Num2UTC}}
#'
#' @examples
#' # Convert UTC to numeric values
#' x <- c ("UTC+09:30", "UTC-07:00")
#'
#' x1 <- UTC2Num (x)
#'
#' print (x)
#'
#'
#' @keywords UTC offset conversion
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
    mp <-  vapply (x, function (i) {
        ifelse (grepl ("-", i), -1, 1)
        }, numeric (1))

    a <- gsub ("\\+|\\-", "", x) #### Remove plus minus sign

    b <- as.numeric (gsub (":.*", "", a))
    c <- as.numeric (gsub (".*:", "", a)) / 60

    # Step 3: Combine to get numeric values -------------------------
    x <- mp * (b + c)

    return (x)
}




#' @title Helper converting the portable operating system interface to offset
#'
#' @param x Character string of POSIX Time or offset (e.g., "+0930")
#'
#'
#' @return Numeric values of UTC offsets
#'
#'
#' @examples
#' .offset2Num (c ("+0930", "-0700"))
#'
#'
#' @noRd

.offset2Num <- function (x) {

  if (inherits (x, "POSIXct") || inherits (x, "POSIXlt"))
    x <- format (x, "%z")

    Sign <- sign (as.numeric(x))
    num  <- abs (as.numeric (substr (x, 1, 3))) +
            abs (as.numeric (substr (x, 4, 5))) / 60

    Ecl <- Sign == 0
    num <- ifelse (Ecl, 0, Sign * num)
    return (num)
}
