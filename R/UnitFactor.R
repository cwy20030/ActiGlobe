#  File ActiGlobe/R/UnitFactor.R
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
#' @title Helper Function to Compute Denominators
#'
#' @description
#' `UnitFactor()` is a helper function that computes the denominator
#' (conversion factor) associated with a given unit. At present, it supports
#' time units and returns the number of seconds corresponding to "day",
#' "hour", "minute", or "second". This denominator can be used in
#' fractional equations, for example to express elapsed time as a fraction
#' of a day or hour.
#'
#' @param x Character string input.
#' @param method Character string specifying the conversion domain.
#'        Defaults to "Time". Currently only "Time" is implemented.
#'
#' @details
#' Supported time units include:
#' \itemize{
#'   \item day returns 86400 (seconds in a day)
#'   \item hour returns 3600 (seconds in an hour)
#'   \item minute returns 60 (seconds in a minute)
#'   \item second returns 1  (seconds in a second)
#' }
#'
#' If an invalid unit is provided, the function calls `Demand()` to prompt
#' the user interactively to select a valid unit.
#'
#' @returns A numeric denominator factor that can be used in fractional
#' calculations.
#'
#' @examples
#' # Denominator for fractions of an hour
#' TUnit <- "hour"
#' UnitFactor(x, method = "Time")
#' # Returns: 3600
#'
#' # Denominator for fractions of a day
#' TUnit <- "day"
#' UnitFactor(x, method = "Time")
#' # Returns: 86400
#'
#' @keywords denominator fraction helper utility
#' @noRd

UnitFactor <- function(x, method = "Time") {
  # Check Point --------------------
  if (!method %in% c("Time")) {
    stop("[UnitFactor] Unsupported method. Currently only 'Time' is
         implemented.")
  }


  # Preprocess input -------------
  x <- tolower(x)


  # Functional Switcher ------------
  ## Time --------------------------------
  if (method == "Time") {
    ### Compute denominator factor based on TUnit --------
    TDivider <- ifelse(
      x == "day", 24 * 3600,
      ifelse(x == "hour", 3600,
        ifelse(x == "minute", 60,
          ifelse(x == "second", 1, NA)
        )
      )
    )

    ### Fallback: prompt user if invalid unit ------------------
    if (is.na(TDivider)) {
      x <- Demand(c("day", "hour", "minute", "second"), "Time Unit")
      TDivider <- ifelse(
        x == "day", 24 * 3600,
        ifelse(x == "hour", 3600,
          ifelse(x == "minute", 60,
            ifelse(x == "second", 1, NA)
          )
        )
      )
    }

    ### Store Ouput --------------------
    Out <- TDivider
  }

  return(Out)
}
