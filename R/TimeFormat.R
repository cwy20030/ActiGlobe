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
#' This function will automatically detect possible time format.
#' Users can choose to either reformat the date or report the
#' datetime format detected.
#'
#'
#' @param Time A character string of time.
#' @param as.time A binary operator indicating whether to return a
#'   converted time based on the detection or the time format.
#'   (default: FALSE, which returns time format)
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
#' \dontrun{
#' # Example 1: When all dates have the same format
#' ## Create and store a date in a variable called Time
#' Time <- c ("2017/05/02 23:00:01", "1970/01/02 05:10:33",
#'            "2000/02/28 07:00:00")
#'
#' ## Ask TimeFormat to tell us the format of the time.
#' TimeFormat (Time, as.time = FALSE)
#'
#' # Example 2: When multiple formats co-exist in a variable
#' ## Create and store dates and time in a variable called Time
#' Time <- c ("2017/05/02 23:00:01", "2000/02/28 07:00",
#'            "1970/01/02", "2022/11/28 08:35 PM")
#'
#' ## Ask TimeFormat to tell us the format of the Time.
#' ### DO This!
#' #### Option 1.
#' print (lapply (Time, TimeFormat))
#'
#' #### Option 2. To match the warning message to the items
#' for (x in Time) {
#'     print (TimeFormat (x))
#' }
#'
#'
#' ### DO NOT!
#' ### print(TimeFormat(Time))
#' ### Note, this process will fail because there are multiple formats
#' }
#'
#' @export

TimeFormat <- function (Time, as.time = FALSE) {
    # Define a vector of time formats
    fmts <- c ("%H:%M:%S", "%I:%M:%S %p", "%H:%M", "%I:%M %p")


    # Unify to Date-Time format
    DateF <- suppressWarnings (tryCatch (DateFormat (Time, as.date = F)))


    #### If the string contains date, remove it.
    if (!DateF == "") {
        #### Extract DateFormat
        D <- tryCatch (
            format (as.POSIXct (Time), DateF)
        )

        # Remove the date part from the time string
        T <- gsub (paste0 (unique (D), collapse = "|"), "", Time)
    } else {
        T <- Time
    }


    # Remove any leading or trailing whitespace
    T <- trimws (T)


    # Test Time Formats
    formatedTime <- strptime (T, format = fmts)

    ## Remove Unmatched Time Formats
    Format <- fmts [which (!is.na (formatedTime))]

    #  TFormat <- unlist(lapply(formats, function(x){
    #    # Try to convert the time string to a POSIXct object
    #    time <- strptime(Time, format = x)

    #    time = ifelse(!is.na(time), x, NA)
    #    if(!is.na(time)) {
    #      return(x)
    #    } else {
    #      return(NA)
    #    }
    #
    #  }))
    #
    #  TFormat = na.omit(TFormat)

    Length <- nchar (Format [!is.na (Format)]) ### Get String Length after Removing NA

    if (length (Length) == 0) {
        TFormat <- NA
        warning ("No time format matched! Please, specify the time format.")
        # Add a function to allow users to add/update new format.
    } else {
        TFormat <- Format [which.max (Length)]
    }


    if (!as.time) {
        ### Return time format
        return (TFormat)
    } else {
        # Test Time Formats

        for (i in seq_along (T)) {
            if (!T [[i]] == "") {
                x <- TimeFormat (T [[i]], as.time = F)


                T [[i]] <-
                    invisible (
                        format (strptime (T [[i]], x), TFormat)
                    )
            }
        }

        return (T)
    }
}
