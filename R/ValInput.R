#  File ActiGlobe/R/ValInput.R
#
#  Copyright (C) 2025  C. William Yao, PhD
#
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
#' @title Validate and Convert Input Vectors for Activity or Time
#'
#' @description
#' A generic helper function that validates and converts input vectors
#' depending on the specified type. Supported types are "Act" for activity
#' values and "Tm" for time coordinates.
#'
#'
#' @details
#' - For \code{type = "Act"}:
#'   * Converts non-numeric input to numeric.
#'   * Stops if all values are zero.
#'   * Stops if any values are `NA`, `NaN`, or `Inf`.
#'
#' - For \code{type = "Time"}:
#'   * Converts non-numeric input via \code{\link{C2T}} with `Discrete = TRUE`.
#'   * Uses OS-specific dispatch (`Darwin`, `Linux`, `Windows`) via
#'   `Sys.info ()[["sysname"]]`.
#'   * Stops if any values fall outside the range `[0, 24]`.
#'
#'
#' @param x A vector of values to validate and convert.
#' @param type Character string specifying the input type. Available options
#' include:
#' \itemize{
#'  \item "Act": Activity counts, which should be numeric and non-negative.
#'  \item "Time", "Date", "DT": Time, date or date-time coordinates, which
#'  should be a vector of character strings, or \code{\link{as.POSIXct}} or
#'  \code{\link{as.POSIXlt}} objects. (e.g., "13:45:00", "1971-01-20" or
#'  "2021-03-05 12:00:00")
#'  \item "TZ": Time zone identifiers, which should be a vector of character
#'  strings available in the IANA database.
#'  }
#' @param AllZero Logical scalar. If `TRUE` (default), the function will check
#' if all values in `x` are zero (applicable only for `type = "Act"`).
#' @param SplitDT Logical scalar. If `TRUE` (default), date and time will be
#' split into two components after unifying the date time formate. If `FALSE`
#' (default), unified date-time format will be returned.
#'
#'
#' @return A validated and converted numeric vector.
#'
#'
#' @seealso \code{\link[base]{Sys.info}}
#'
#'
#' @examples
#' # Activity validation
#' ValInput (c ("1", "2", "3"), type = "Act")
#'
#' # Time validation (requires C2T defined)
#' ValInput (c ("12:00", "13:00"), type = "Time")
#'
#' # Date validation (requires DateFormat defined)
#' ValInput (c ("2021-01-20", "2021-02-15"), type = "Date")
#'
#' # Date-time validation (requires DateFormat and TimeFormat defined)
#' ValInput (c ("2021-03-05 12:00:00", "2021-03-05 13:00:00"), type = "DT")
#'
#'
#' @noRd

ValInput <- function (x, type = c ("Act", "Time", "Date", "DT", "TZ"),
                      AllZero = TRUE,
                      SplitDT = FALSE) {
    # Step 0 Argument matching and system info Extraction ---------------------
    type <- match.arg (type)

    # Step 1 Switch based on type ---------------------------------------------
    switch (type,

            ## Activity validation ---------------------
            "Act" = {
                if (!inherits (x, "numeric"))
                    x <- as.numeric (as.character (x))
                if (any (c (!is.finite (x)), all (c (AllZero, x == 0))))
 stop ("Invalid activity values! The measures likely contain:
  1. NA/NaN/Inf
  2. only zeros

  Please, verify and rerun the function.")
                return (x)
            },


            ## Time validation ---------------------
            "Time" = {
                Out <- .ValDT (x, type = "Time")
                return (Out)
            },


            ## Date validation ------------------------
            "Date" = {
                Out <- .ValDT (x, type = "Date")
                return (Out)
            },

            ## Date-time validation ---------------------
            "DT" = {
                Out <- .ValDT (x, type = "DT", SplitDT = SplitDT)
                return (Out)
            },
            # Time Zone ------------------------
            "TZ" = {
                if (!inherits (x, "character"))
                    stop ("Time zone identifiers should be character strings.")

                if ("local" %in% tolower (x))
                    x [x == "local"] <- Sys.timezone ()


                # Check if time zones are valid
                ValidTZ <- tolower (OlsonNames ())
                Invld   <- !tolower (x) %in% ValidTZ


                  if (any (Invld)) {

                      if (any (x %in% iIANA$TZ_Code))
                          x [x %in% iIANA$TZ_Code] <-
                            iIANA$TZ_IANA [match (x, iIANA$TZ_Code)]


                      Invld2 <- !tolower (x) %in% ValidTZ

                      if (any (Invld2)) {
                          stop (paste0 (
                              "Invalid time zone identifier(s) detected: \n",
                              paste0 (
                                  paste (x [Invld2], collapse = ", "),
                                  " \n\n"),
                                  "Please check against the IANA time zone
                                  database using the offline IANA database
                                  in the package `View(IANA)`. Alternatively,
                                  `Geo2TZ()` can assist converting city
                                  name to time zone identifiers."))
                            }
                      }


                return (x)
            }
    )
}



.ValDT <- function (x, type = c ("Time", "Date", "DT"), SplitDT = FALSE) {
    # Step 1 Switch based on type ---------------------------------------------
    switch (type,

            ## Time validation ---------------------
            "Time" = {
                if (any (is.na (x)))
stop (paste0 ("NAs detected in time"))

                if (!inherits (x, "numeric")) {
                    Out <- vapply (x, function (xx) {
                        C2T (xx, Discrete = TRUE)
                    },
                    FUN.VALUE = numeric (1)
                    )
                } else {
                    Out <- x
                }

                if (any (c (Out > 24, Out < 0))) {
 stop ("Time coordinates should reflect the clock time units.
 Please, verify and rerun the function.")
                }
                return (Out)
            },


            ## Date validation ------------------------
            "Date" = {
                if (!inherits (x, c ("POSIXct", "POSIXlt",
                                     "character", "Date")))
stop ("The input date object should be class of character or POSIX
date-time objects! Please, verify and rerun the function.")

                # Remove time component
                Ds <- tryCatch (
                    sub ("\\s.*$", "", as.character (x))
                )

                Out     <- DateFormat (Ds, as.date = TRUE)
                Out     <- as.character (Out)
                if (all (is.na (Out)))
stop ("Unable to convert date object. Please check your input.")

                return (Out)
            },

            ## Date-time validation ---------------------
            "DT" = {
                ### Do not worry about time zone because the values should be
                ### adjusted already in ActiGlobe.
                if (!inherits (x, c ("POSIXct", "POSIXlt",
                                     "character")))
stop ("The input date-time object should be class of character or POSIX
date-time objects! Please, verify and rerun the function.")

                    ### Check the format
                    DForm <- DateFormat (x, as.date = FALSE)
                    TForm <- TimeFormat (x, as.time = FALSE)

                    if (any (c (is.na(DForm), is.na (TForm))))
stop ("Unable to parse date-time format. Please check your input.")

                    if (SplitDT) {
                        ## Date
                        Ds <- tryCatch (
                            format (strftime (x, DForm))
                        )
                        Ds <- as.Date (Ds, format = DForm)

                        ## Time
                        Tm <- tryCatch (
                            format (strftime (x, TForm))
                        )
                        Hr <- vapply (Tm, function (xx)
                            C2T (xx, Discrete = TRUE),
                            FUN.VALUE = numeric (1)
                        )

                        ## Check Conversion
                        if (any (c (all (is.na (Ds)),
                                    all (is.na (Hr))))) {
stop ("Unable to convert date-time object.
Please, manually check date and time format using `DateFormat()` and
`TimeFormat()`. See package vignette for further tutorial on the uses
of these funcitons.")
                        }

                        ## Return
                        x <- data.frame (Date = Ds,
                                         Time = Tm,
                                         Hour = Hr)
                    } else {
                        x <- format (x, format = paste0 (DForm, " ",TForm))
                    }
                    return (x)
                }

    )
}
