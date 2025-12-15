#  File ActiGlobe/R/DateFormat.R
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
#' @title Detect Possible DT Format
#'
#' @description
#' This function will automatically detect possible date format. Users can
#' choose to either reformat the date or report the datetime format detected.
#'
#' @title Format Recognition and Conversion of Date-Time Strings
#'
#' @description
#' Identifies the likely date format used in character strings and
#' optionally converts these strings to Date objects. This utility is
#' intended to support pre-processing of timestamp data with varied or
#' unknown formatting. When ambiguity arises (e.g., day-month versus
#' month-day ordering), the function defaults to the first compatible format
#' unless explicitly guided by a delimiter or by manual correction.
#'
#' @param DT A character vector containing date or date-time strings.
#' @param as.date Logical. If TRUE (default), returns converted Date objects.
#'   If FALSE, returns the detected format string (e.g., \code{"\%d/\%m/\%Y"}).
#' @param Delim Optional. A single character (e.g., \code{"."}, \code{"-"},
#'   or \code{"/"}) to override default date delimiters. Requited when input
#'   strings may contain nonstandard separators (e.g., \code{"$"}, \code{"~"}).
#'
#' @return
#' If \code{as.date = TRUE}, returns a vector of class \code{Date}.
#'
#' If \code{as.date = FALSE}, returns a character string representing the
#' detected format. When multiple formats coexist or no valid format is
#' detected, warnings are issued and default behavior is applied.
#'
#' If no format matches, returns \code{NA} and issues a warning.
#'
#' @examples
#' # Consistent format across all strings
#' DT <- c("2017/05/02", "2000/02/28", "1970/01/02")
#' DateFormat(DT, as.date = FALSE) # returns parsed Date vector
#'
#' \dontrun{
#' # Mixed formats within a vector
#' DT <- c(
#'   "2017/05/02", "2000.Feb.28", "1970-11-02",
#'   "January 01, 2025", "December 12, 1980"
#' )
#' lapply(DT, DateFormat) # element-wise parsing
#' ### We expect that many of these format will not work because
#' ### they contain text
#'
#'
#' DT <- c(
#'   "2017/05/02", "2000.02.28", "1970-11-02",
#'   "01, 01, 2025", "12, 12, 1980"
#' )
#'
#' # Recommended usage for mixed formats:
#' lapply(DT, DateFormat, Delim = ",") # element-wise parsing
#'
#' for (x in DT)
#' print(DateFormat(x, Delim = ",")) # displays format/warning per entry
#'
#' # Avoid using sapply, because it will convert them into numeric form
#' sapply(DT, DateFormat)
#' }
#'
#' @export


DateFormat <- function(DT, as.date = TRUE, Delim = NULL) {
  fmts <- c(
    "%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
    "%Y-%m", "%m-%Y",
    "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y",
    "%Y/%m", "%m/%Y",
    "%Y.%m.%d", "%m.%d.%Y", "%d.%m.%Y",
    "%Y.%m", "%m.%Y"
  ) # Add more formats as needed

  if (!is.null(Delim)) {
    Base <- c(
      "%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
      "%Y-%m", "%m-%Y"
    )
    Base <- gsub("-", Delim, Base) # Only one deliminator is allowed.
    fmts <- c(fmts, Base)
  }

  formatedDate <- tryCatch(as.Date(DT, format = fmts))

  Format <- fmts[which(!is.na(formatedDate))] # Extract Possible format

  ## Extract the time format generated
  formatedDate <- formatedDate[!is.na(formatedDate)]

  ## See which time format works for this
  if (length(Format) > 1) {
    Format <- unlist(lapply(seq_len(length(formatedDate)), function(d) {
      temp <- as.character(formatedDate[d])
      temp <- unlist(strsplit(temp, split = "-"))

      if (all(as.numeric(temp) < 1000)) {
        NA
      } else {
        Format[d]
      }
    }))

    Format <- Format[!is.na(Format)]
  }

  if (!as.date) {
    # Post-process Check...
    if (length(Format) == 0) {
      warning(paste0("Possible illegal datetime format detected in ", DT, ".
      Please, ensure that...
         1. the year is recorded as full four digits (i.e., 19xx).
         ==> Please, manually correct the year and try again.

         2. the deliminator used to separate month and date is not of standard.
         ==> Please, specify the proper deliminator in Delim."))

      Format <- ""
    }

    if (length(unique(Format)) > 1) {
      warning("Unable to recognize the difference between date and month.
              Only the first detected format would be used! Please, manually
              set it using as.Date function if it is incorrect or convert the
              month number to name.")
    }
  }


  Format <- Format[1]
  if (as.date) {
    return(as.Date(DT, format = Format))
  } else {
    return(Format)
  }
}
