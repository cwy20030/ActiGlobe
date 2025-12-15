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
#' @title Validate and convert input vectors for activity or time
#'
#' @description
#' A generic helper function that validates and converts input vectors
#' depending on the specified type. Supported types are `"Act"` for activity
#' values and `"Tm"` for time coordinates.
#'
#' @param x A vector of values to validate and convert.
#' @param type Character string specifying the input type. Must be either
#'   `"Act"` or `"Tm"`.
#'
#' @details
#' - For `type = "Act"`:
#'   * Converts non-numeric input to numeric.
#'   * Stops if all values are zero.
#'   * Stops if any values are `NA`, `NaN`, or `Inf`.
#'
#' - For `type = "Tm"`:
#'   * Converts non-numeric input using `C2T ()` with `Discrete = TRUE`.
#'   * Uses OS-specific dispatch (`Darwin`, `Linux`, `Windows`) via
#'   `Sys.info ()[["sysname"]]`.
#'   * Stops if any values fall outside the range `[0, 24]`.
#'
#' @return A validated and converted numeric vector.
#'
#' @examples
#' # Activity validation
#' ValInput (c ("1", "2", "3"), type = "Act")
#'
#' # Time validation (requires C2T defined)
#' # ValInput (c ("12:00", "13:00"), type = "Tm")
#'
#' @noRd


ValInput <- function (x, type = c ("Act", "Tm")) {
    # Step 0 Argument matching and system info Extraction ---------------------
    type <- match.arg (type)

    # Step 1 Switch based on type ---------------------------------------------
    switch (type,

        ## Activity validation ---------------------
        "Act" = {
            if (!inherits (x, "numeric")) x <- as.numeric (as.character (x))
            if (all (x == 0)) stop ("All activity values are zero.")
            if (any (!is.finite (x))) stop ("Activity contains NA/NaN/Inf.")

            return (x)
        },


        ## Time validation ---------------------
        "Tm" = {
            if (!inherits (x, "numeric")) {
                x <- vapply (x, function (xx) {
                    C2T (xx, Discrete = TRUE)
                },
                FUN.VALUE = numeric (1)
                )
            }

            if (any (x > 24 | x < 0)) {
                stop (paste (
                    "Currently, the model cannot fit actigraphy
                    recordings lasting longer than a day.",
                    "Please, rescale the time coordinate to between 0 and 24.",
                    "Note that it is crucial to have the proper time
                    coordinate since the model relies on it."
                ))
            }
            return (x)
        }
    )
}
