#  File ActiGlobe/R/TimeFormat.R
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
#' @title Detect Possible Time Format
#'
#' @description
#' This function will automatically detect possible time format. Users can
#' choose to either reformat the date or report the datetime format detected.
#'
#'
#' @param time A character string of time.
#' @param as.time A binary operator indicating whether to return a converted
#' time based on the detection or the time format. (default: FALSE, which
#' returns time format)
#'
#' @returns
#' If \code{as.time = TRUE}, returns a character vector of the input times
#' reformatted according to the detected format. Each element corresponds to
#' the respective entry in \code{Time}.
#'
#' If \code{as.time = FALSE}, returns a character scalar giving the
#' best-matching time format string.
#'
#' If no format matches, returns NA and issues a warning.
#'
#' @examples
#'
#' # Example 1: When all dates have the same format
#' ## Create and store a date in a variable called Time
#' Time <- c("2017/05/02 23:00:01", "1970/01/02 05:10:33",
#' "2000/02/28 07:00:00")
#'
#' ## Ask TimeFormat to tell us the format of the time.
#' TimeFormat(Time, as.time = FALSE)
#'
#' # Example 2: When multiple formats co-exist in a variable
#' ## Create and store dates and time in a variable called Time
#' Time <- c("2017/05/02 23:00:01", "2000/02/28 07:00", "1970/01/02",
#' "2022/11/28 08:35 PM")
#'
#' ## Ask TimeFormat to tell us the format of the Time.
#' ### DO This!
#' #### Option 1.
#' print(lapply(Time, TimeFormat))
#'
#' #### Option 2. To match the warning message to the items
#' for (x in Time) {
#'   print(TimeFormat(x))
#' }
#'
#'
#' ### DO NOT!
#' ### print(TimeFormat(Time))
#' ### Note, this process will fail because there are multiple formats
#'
#'
#' @export

TimeFormat <- function(time, as.time = FALSE) {
  # Define a vector of time formats
  fmts <- c("%H:%M:%S", "%I:%M:%S %p",
            "%H:%M", "%I:%M %p")

  ## Control for microsecond
  fmts.micro <- c("%H:%M:%OS", "%I:%M:%OS %p")

  ### Check if any time string has period symbol
  if (any(grepl("\\.", time))) {
        idx <- grep("\\.", time)

        ### Break up parts of the time string by colon
        parts <- strsplit(time[idx], ":")
        lens <- lengths(parts)


        if (any(lens == 3)) {
          Sidx <- which(lens == 3)

          FractP <- vapply( ### Any has fractional part in seconds
            Sidx,
            function(i) grepl("\\.", parts[[i]][3]),
            logical(1)
            )

          if (any(FractP)) {
            fmts <- c(fmts, fmts.micro)
          }


        }
        }


  # Unify to Date-Time format
  DateF <- suppressWarnings(tryCatch(DateFormat(time, as.date = FALSE)))



  #### If the string contains date, remove it.
  if (!DateF == "") {
    #### Extract DateFormat
    D <- tryCatch(
      format(as.POSIXct(time), DateF)
    )

    # Remove the date part from the time string
    Tm <- gsub(paste0(unique(D), collapse = "|"), "", time)
  } else {
    Tm <- time
  }


  # Remove any leading or trailing whitespace
  Tm <- trimws(Tm)


  # Test Time Formats
  formatedTime <- strptime(Tm, format = fmts)

  ## Remove Unmatched Time Formats
  Format <- fmts[which(!is.na(formatedTime))]

  ### Get String Length after Removing NA
  Length <- nchar(Format[!is.na(Format)])

  if (length(Length) == 0) {
    TFormat <- NA
    warning("No time format matched! Please, specify the time format.")
    # Add a function to allow users to add/update new format.
  } else {
    TFormat <- Format[which.max(Length)]
  }


  if (!as.time) {
    ### Return time format
    return(TFormat)
  } else {
    # Test Time Formats

    for (i in seq_along(Tm)) {
      if (!Tm[[i]] == "") {
        x <- TimeFormat(Tm[[i]], as.time = FALSE)


        Tm[[i]] <-
          invisible(
            format(strptime(Tm[[i]], x), TFormat)
          )
      }
    }

    return(Tm)
  }
}
