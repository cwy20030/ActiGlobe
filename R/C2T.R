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
#' @param time A vector of time values. Can be numeric, character, or factor.
#'   Examples include `"13:45:00"` or `"01:30:15"`. If numeric, values are
#'   returned as-is (after coercion). If character/factor, values are parsed
#'   using \code{\link{TimeFormat}}.
#' @param Discrete Logical scaler; if TRUE, each input in `Time` is converted
#'   individually without subtraction for the prior time. Default FALSE will
#'   subtract from the first time point.
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
#' times <- c("01:00:00", "02:30:00", "03:15:00")
#' C2T(times, Discrete = TRUE)
#'
#' @noRd

C2T <- function(time, Discrete = FALSE) {
  # Check Point ------------------------
  if (any(grepl("^[A-Za-z]+$", time)) || any(is.numeric(time))) {
    stop("Input 'time' must be a pure character time string with no
    timezone label. Please, check the input using TimeFormat().")
  }


  # First Attempt to Coerce to Numeric -------------
  x <- suppressWarnings(as.numeric(as.character(time)))

  # If All NAs, Parse as Time Strings -------------
  if (length(na.omit(x)) == 0) {
    Fmt <- TimeFormat(time, as.time = FALSE)

    decimal_hours <- vapply(time, function(Tm)
      ParseT(time = Tm, fmt = Fmt),
      numeric(1)
    )


    x <- as.numeric(decimal_hours)
  }

  # Adjust for Initial Time if Needed -------------
  if (!Discrete) {
    ini <- x[[1]]
    x <- x - ini ## For duration
  }

  if (any(x < 0) || any(x > 24)) {
    stop("Negative or uut of range (0-24) value detected.
              Please check your input 'time' values.")
  }


  if (any(is.na(x))) warning(paste0("NAs introduced by coercion"))


  # Return Result ------------------------
  return(x)
}


#' @title Parse Time Strings into Decimal Hours
#'
#' @param time A character vector of time strings (e.g., "13:45:00").
#' @param fmt A character string specifying the format of the time strings,
#'
#' @noRd

ParseT <- function(time, fmt) {
  # Step 1. Check if "%I" exists (12-hour clock)
  is12 <- grepl("%I", fmt)
  has_ampm <- grepl("AM|PM", time, ignore.case = TRUE)

  # Step 2. Count how many ":" exist
  colon_count <- lengths(regmatches(time, gregexpr(":", time)))

  # Step 3. Split the time string by ":"
  parts <- unlist(strsplit(gsub("AM|PM", "", time, ignore.case = TRUE), ":"))

  # Step 4. Extract Hour, Minute, Second
  hour   <- as.numeric(parts[1])
  minute <- ifelse(colon_count >= 1, as.numeric(parts[2]), 0)
  second <- ifelse(colon_count >= 2, as.numeric(parts[3]), 0)

  # Adjust for 12-hour format if needed
  if (is12 && has_ampm) {
    if (grepl("PM", time, ignore.case = TRUE) && hour < 12) hour <- hour + 12
    if (grepl("AM", time, ignore.case = TRUE) && hour == 12) hour <- 0
  }

  # Return decimal hours
  Out <- hour + minute/60 + second/3600

  return(Out)
}
