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
#' @title Format Recognition and Conversion of Date Strings
#'
#' @description
#' Identifies the likely date format used in character strings and
#' optionally converts these strings to Date objects. This utility is
#' intended to support pre-processing of timestamp data with varied or
#' unknown formatting. When ambiguity arises (e.g., day-month versus
#' month-day ordering), the function defaults to the first compatible format
#' unless explicitly guided by a delimiter or by manual correction.
#'
#' @importFrom lifecycle badge
#'
#' @param Date A character vector containing **date** OR **date-time** strings.
#' @param as.date Logical. If TRUE (default), returns converted Date objects.
#'   If FALSE, returns the detected format string (e.g., \code{"\%d/\%m/\%Y"}).
#' @param Delim Optional. A single character (e.g., \code{"."}, \code{"-"},
#'   or \code{"/"}) to override default date delimiters. Requited when input
#'   strings may contain nonstandard separators (e.g., \code{"$"}, \code{"~"}).
#' @param Guess Logical. If TRUE, the function will attempt to guess
#' the delimiter used in the input strings if not provided. If FALSE (default),
#' it will rely solely on the specified \code{Delim} or default delimiters.
#' @param DT `r lifecycle::badge("deprecated")` Use `Date` instead.
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
#' @seealso \code{\link{DateFormat}} \code{\link{as.posIXct}}
#'
#' @examples
#' # Case 1. Detect date format from the string:
#' ## Consistent format across all strings
#' ### We will store all these strings in a variable called DT, short for
#' ### date-time.
#' DT <- c("2017/05/02", "2000/02/28", "1970/01/02")
#' DateFormat(DT, as.date = FALSE) # returns parsed Date vector
#'
#' # Caution:
#' ## It would likely fail if the vector contains
#' ## mixtures of string and numeric values.
#' DT1 <- c(
#'     "2017/05/02", "2000.Feb.28", "1970-11-02",
#'     "January 01, 2025", "December 12, 1980"
#' )
#' lapply(DT1, DateFormat, as.date = FALSE) # element-wise parsing
#'
#'
#' # version > 0.3.0:
#' ## As of version 0.3.0 DateFormat can now bulk detect mixed date formats
#' DT2 <- c(
#'     "2017/05/02", "2000.02.28", "1970-11-02",
#'     "01, 01, 2025", "12, 12, 1980"
#' )
#' DateFormat(Date = DT2, Guess = TRUE)
#'
#'
#'
#' # Case 2. Convert string to date object:
#' ## version > 0.3.0:
#' ### As of version 0.3.0 DateFormat can now bulk convert date strings with
#' ### mixed format by using the `Guess` argument to detect the deliminator.
#'
#' DateFormat(Date = DT2, as.date = TRUE, Guess = TRUE)
#'
#' # *** Users should still be caution about the order of date and month
#' in the converted date string, because it is likely to be misinterpreted
#' when the the numeric month value is less than 13.
#'
#'
#' ## version < 0.3.0
#' ### list-based element-wise parsing:
#' lapply(DT2, DateFormat, Delim = ",")
#'
#' ### vector-based serial-processing:
#' for(x in DT2){
#'     print(
#'     DateFormat(x, Delim = ","))
#' } # displays format/warning per entry
#'
#'
#' # *** Avoid using sapply, because it will turn the date into a
#' numeric value
#' sapply(DT, DateFormat)
#'
#' @keywords date convert format
#' @export


DateFormat <- function (Date, as.date = TRUE, Delim = NULL, Guess = TRUE,
                        DT = NULL) {

  # Step 0. Input Validation and Parameter Extraction -----------
  if (all (c (is.null (Date), !is.null (DT)))) {
    lifecycle::deprecate_soft (
      when = "0.3.1",
      what = "DateFormat(DT)",
      with = "DateFormat(Date)"
    )
    Date <- DT
  }
  Date <- sub ("\\s.*$", "", as.character (Date))

  # Step 1.x Optional Step ----------------------------------
  if (Guess) {
    x <- unlist (
      regmatches (Date, gregexpr ("[^0-9A-Za-z]+", Date))
    )
    x <- unique (x)

    x <- x [!x %in% ":"]

    if (length (unique (x)) > 1) {
      warning ("Multiple deliminators detected in the input.
               The first detected symbol of each substring
               will be assumed as the deliminator.")

      if (is.null (Delim)) {
        Delim <- x
      } else {
        Delim <- unique (c (x, Delim))
      }
    }
  }

  # Step 2 Build Format List ---------------
  fmts <- c (
    "%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
    "%Y-%m", "%m-%Y",
    "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y",
    "%Y/%m", "%m/%Y",
    "%Y.%m.%d", "%m.%d.%Y", "%d.%m.%Y",
    "%Y.%m", "%m.%Y"
  ) # Add more formats as needed

  if (!is.null (Delim)) {
    Base <- c (
      "%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
      "%Y-%m", "%m-%Y"
    )

    fmts_new <- unlist (
      lapply (Delim, function (d) gsub ("-", d, Base, fixed = TRUE)),
      use.names = FALSE
    ) # Only one deliminator is allowed.

    fmts <- unique (c (fmts, fmts_new))
  }

  # Step 3. Test Date Formats ---------------
  if (length (Date) == 1) {
    formatedDate <- tryCatch (as.Date (Date, format = fmts))
    Format <- fmts [which (!is.na (formatedDate))]

  } else {
    ## Test Date Formats
    DFormat <-
      vapply (
        fmts,
        function (fmt) as.Date (Date, format = fmt),
        FUN.VALUE = as.Date (rep (NA, length (Date)))
      )
    ## Remove Unmatched Date Formats
    formatedDate <- DFormat [, colSums (!is.na (DFormat)) > 0, drop = FALSE]

    ## Remove columns with all negative values (invalid date formats)
    formatedDate <-
      formatedDate [,
                    !apply (formatedDate, 2, function (col) {
                      v  <- as.numeric (col)
                      nv <- v [!is.na (v)]

                      if (length (nv) == 0) FALSE else all (nv < 0)
                    }), drop = FALSE]


    ## Check if any fulfills all
    nNA <- colSums (is.na (formatedDate)) == 0

    if (any (nNA)) {
      Format <- colnames (formatedDate) [nNA]
    } else {

      Format <- colnames (formatedDate)

      ## See which time format works for this
      if (length (Format) > 1) {
        Format <- vapply(
        seq_len (nrow (formatedDate)),
        function (i) {
          row <- formatedDate [i, ]
          nn <- names (row) [!is.na (row)]

          if (length (nn) == 0)
            return (NA_character_)


          if (length (nn) > 1) {
            warning (sprintf(
              "Row %d has multiple possible formats: %s. Using the first.",
              i, paste (nn, collapse = ", ")
            ))
          }

          nn [1]
        },
        FUN.VALUE = character (1)
      )
    }
    }
    }

    if (!as.date) {
        # Post-process Check...
        if (length (Format) == 0) {
            warning (paste0 ("Possible illegal datetime format detected in ",
            Date, ".
      Please, ensure that...
         1. the year is recorded as full four digits (i.e., 19xx).
         ==> Please, manually correct the year and try again.

         2. the deliminator used to separate month and date is not of standard.
         ==> Please, specify the proper deliminator in Delim."))

            Format <- ""
        }

        if (length (unique (Format)) > 1) {
            warning ("Unable to recognize the difference between date and month.
              Only the first detected format would be used! Please, manually
              set it using as.Date function if it is incorrect or convert the
              month number to name.")
        }
    }
  # Step 4. Conversion & Return ---------------
    if (as.date) {
        x <- as.Date (Date, format = Format)
        class (x) <- "Date"
        return (x)
    } else {
        return (Format)
    }
}
