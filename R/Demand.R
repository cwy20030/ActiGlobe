#  File ActiGlobe/R/Demand.R
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
#' @title Interactive Option Selector
#'
#' @description
#' The `Demand()` function presents a list of options to the user, prompts them
#' to select one by entering its number, and returns the chosen option. If the
#' option contains the word `"Other"`, the user is asked to provide a custom
#' input (e.g., a path). Invalid selections trigger a retry until a valid choice
#' is made.
#'
#' @details
#' - Displays each option with its index number.
#' - Prompts the user to select by entering the number.
#' - Validates input: must be numeric and within the range of options.
#' - If `"Other"` is selected, prompts for a custom string (e.g., a path).
#' - Recursively re-prompts until a valid selection is made.
#'
#' @param options A character vector of options to present to the user.
#' @param MESSAGE A descriptive string used in the prompt (e.g., `"file"`,
#'   `"directory"`, `"method"`).
#'
#' @return A single character string corresponding to the selected option or
#'   user-provided input if `"Other"` is chosen.
#'
#' @examples
#'
#' Demand(c("Option A", "Option B", "Other"), "option")
#'
#' @keywords prompt ask demand
#' @noRd

Demand <- function (options, MESSAGE) {
    # Print the options for the user
    for (i in seq_len (length (options))) {
        cat (paste (i, ": ", options [i], "\n", sep = ""))
    }

    # Request the user to select an option
    selected_option <-
        readline (prompt = paste0 (
            "Please select the ",
            MESSAGE, " by entering its number: "
        ))

    # Check if the input is a number and within the range of options
    if (!grepl ("^[0-9]+$", selected_option) ||
        as.numeric (selected_option) > length (options) ||
        as.numeric (selected_option) < 1) {
        cat ("Invalid selection. Please try again.\n")
        return (Demand (options, MESSAGE))
    }

    if (grepl ("Other", options [as.numeric (selected_option)])) {
        options [as.numeric (selected_option)] <-
            readline (prompt = "Please enter the path: ")
    }


    # Return the selected option
    return (options [as.numeric (selected_option)])
}






#' @title Obtain query suggestions based on input
#'
#' @importFrom httr2 request req_url_query req_user_agent req_timeout
#' @importFrom httr2 req_perform resp_check_status resp_body_string
#' @importFrom jsonlite fromJSON
#'
#' @param Query The search query string.
#' @param URL The base URL for the search API
#' @param UA User agent string for the HTTP request.
#' @param Limit Maximum number of suggestions to return.
#' @param Timeout Request timeout in seconds.
#' @param Translate Logical; whether to translate suggestion labels.
#' @param Lang Target language code for translation (e.g., "en", "es
#'
#' @noRd

Suggest <- function (Query,
                     URL       = "https://nominatim.openstreetmap.org/search",
                     UA        = Default_UA (),
                     Limit     = 8,
                     Timeout   = 20,
                     Translate = TRUE,
                     Lang      = "en") {

    # Request suggestions from the search API -----------------
    Req <-
        httr2::request (URL) |>
        httr2::req_url_query (
            q = Query,
            format = "jsonv2",
            addressdetails = 1,
            limit = Limit
        ) |>
        httr2::req_user_agent (UA) |>
        httr2::req_timeout (Timeout)

    Resp <- httr2::req_perform (Req)
    httr2::resp_check_status (Resp)

    Txt <- httr2::resp_body_string (Resp)
    X   <- jsonlite::fromJSON (Txt)

    # Prepare Output ------------------------------

    if (length (X) == 0) {
        return (data.frame ())
    }

    Lab <- X$display_name

    if (isTRUE (Translate) && !is.null (Lang) && nzchar (Lang)) {
        Lab <-
            vapply (Lab, function (x) {
                Translator (Text = x, Lang = Lang)
            }, FUN.VALUE = character (1))
    }

    # Output -------------------
    data.frame (
        Label = Lab,
        Lat   = as.numeric (X$lat),
        Lon   = as.numeric (X$lon),
        stringsAsFactors = FALSE
    )
}
