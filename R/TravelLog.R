#  File ActiGlobe/R/TravelLog.R
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
#' @title Generate the Travel Log Template Needed for Time Zone Correction
#'
#' @description
#' A helper function to generate a travel log template needed for time zone
#' correction. This function creates a data frame with the necessary columns
#' for users to fill in their travel log information. This is an essential
#' step for analysis involving time zone shifts. We recommend users to consult
#' the package vignette to learn how to use the travel log template.
#'
#' @details
#' The travel log template can be generated in two formats: "UTC" and "CC".
#' The "UTC" (coordinated universal time) template is general recommended as it
#' aims to protect personal privacy of participants without compromising
#' reproducibility. Note that the "CC" (city-country) template should be
#' avoided when analyzing data containing sensitive information.
#'
#' For UTC offset, users may consult the IANA database \code{View(IANA)},
#' already installed with the package. Alternatively, users may also use
#' \code{\link{TZ2UTC}} or \code{\link{Geo2TZ}} to obtain the UTC offset.
#'
#' Spelling check is recommended when using the "CC" template, as mistakes can
#' halt the process of time shift adjustment. It is recommended to keep the
#' documentation in English since accents may not be recognized by the default
#' encoding system depending on the operating system.
#'
#'
#' @importFrom utils write.csv
#'
#'
#' @param Type A character string to indicate the type of template to create.
#' Options are
#' \itemize{
#'  \item "UTC": the default template that requires users to input the UTC
#'  offset (e.g., "UTC +09:30" or "+9.5") of the local or travel destination
#'  time zone.
#'  \item "CC": a template that requires users to input both city and country
#'  names of the location. This template is not suited for analysis involving
#'  sensitive information. In rare cases that multiple cities share the same in
#'  one country, users should input the correct corresponding state or province
#'  names.
#' }
#' @param Write A binary code to indicate whether to write a .csv file
#' containing the template needed for the travel log. (default = FLASE)
#' When set to FALSE, the template will be returned as an object.
#' When set to TRUE, user must provide the target directory where they wish to
#' store the template.
#' @param Dir The directory where the travel log template to be exported <e.g.
#' "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#'
#' @return a travel-log template as a data.frame or written as a CSV file
#'
#' @examples
#'
#' Tlg <- TravelLog (Write = FALSE)
#'
#' print (Tlg)
#'
#' @keywords Travel Log Template Timezone Shift
#' @export

TravelLog <- function (Type = "UTC", Write = FALSE, Dir = NULL) {

    Type <- toupper (Type)

    switch (
        Type,
        "UTC" = {
            data <- data.frame (matrix (ncol = 5, nrow = 1))

            names (data) <- c (
                "ID", "UTC_Offset", "Country_with_Daylight_Saving",
                "date_Start", "date_End"
            )

            data [1, ] <- c (
                "ExampleID", "+05:00", "TRUE",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

        },
        "CC" = {

            data <- data.frame (matrix (ncol = 6, nrow = 2))

            names (data) <- c (
                "ID", "City", "Province_or_State" ,"Country",
                "date_Start", "date_End"
            )

            data [1, ] <- c (
                "ExampleID", "Paris", "" ,"France",
                as.character (Sys.Date () - 15),
                as.character (Sys.Date () - 10)
                )

            data [2, ] <- c (
                "ExampleID", "Paris", "Ontario" ,"Canada",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

        },
        stop ("Invalid Type. Please choose either \"UTC\" or \"CC\".")
    )


    ## Write option
    if (Write) { ### When TRUE

        ### Check essential components
        if (is.null (Dir)) {
            stop ("To export the template, a directory must be provided.")
        }

        write.csv (data, paste0 (Dir, "/TravelLog_Template.csv"),
                   row.names = FALSE)
    }


    if (!Write) {
        return (data)
    }
}
