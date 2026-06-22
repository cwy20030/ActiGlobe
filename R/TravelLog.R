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
#' The travel log template can be generated in two formats: "TZ", "UTC" and
#' "Geo", ranking by their levels of performance and security.
#'
#' \tabular{lcccl}{
#' Type
#'  \tab Security Level
#'  \tab Performance
#'  \tab
#'  \tab Common or Potential Issues \cr
#'
#' \strong{TZ}
#'   \tab Moderate–High
#'   \tab High
#'   \tab
#'   \tab Rare renaming issue \cr
#'
#' \strong{UTC}
#'   \tab High
#'   \tab Moderate–High
#'   \tab
#'   \tab Not suited for travels during DST transition for Zone 2. \cr
#'
#' \strong{Geo}
#'   \tab Poor
#'   \tab Moderate–High
#'   \tab
#'   \tab Spelling errors; high risk of violating localized regulations \cr
#' }
#'
#'
#'
#'
#'
#' \describe{
#' \strong{TZ Template}:
#'
#' The time zone template (default) requires users to input the IANA time zone
#' identifiers of the local or travel destination time zone. This template
#' has the best performance as it allows the most accurate time shift
#' adjustment by directly using the time zone information. It also has
#' the modest security level as certain time zone identifiers may be
#' unique to specific geological location.
#'
#'
#'
#' \strong{UTC Template}:
#' The coordinated universal time template requires users to input the UTC
#' offset of the local or travel destination. This format is generally
#' recommended for analysis of highly sensitive data as it aims to maximize
#' the protection of geological location information. It is worth noting that
#' both indication of local practice of daylight saving time and zone
#' compromising reproducibility.
#'
#'
#' Note that the "Geo" (city-country)
#' template should be avoided when analyzing data containing sensitive
#' information.
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#' For UTC offset, users may consult the IANA database \code{View(IANA)},
#' already installed with the package. Alternatively, users may also use
#' \code{\link{TZ2UTC}} or \code{\link{Geo2TZ}} to obtain the UTC offset.
#'
#' Spelling check is recommended when using the "Geo" template, as
#' mistakes can
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
#'  \item "TZ": a template that requires users to input the IANA time zone
#'  identifiers (e.g., "America/New_York" or "Asia/Tokyo") of the local or
#'  travel destination time zone. This template is recommended for general
#'  usage. Users may use \code{\link{Geo2TZ}} to obtain the IANA time zone
#'  identifiers.
#'  \item "UTC": the default template that requires users to input the UTC
#'  offset (e.g., "UTC +09:30" or "+9.5") of the local or travel destination
#'  time zone.
#'  \item "Geo": a template that requires users to input both city and
#'  country names of the location. This template is not suited for analysis
#'  involving sensitive information. In rare cases that multiple cities share
#'  the same in one country, users should input the correct corresponding
#'  state or province names.
#' }
#' @param Write A binary code to indicate whether to write a .csv file
#' containing the template needed for the travel log. (default = FLASE)
#' When set to FALSE, the template will be returned as an object.
#' When set to TRUE, user must provide the target directory where they wish to
#' store the template.
#' @param Dir The directory where the travel log template to be exported <e.g.
#' "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#'
#'
#' @return a travel-log template as a data.frame or written as a CSV file
#' depending on the value of \code{Write}.
#' \itemize{
#'  \item TZ: a data frame with columns
#'   \itemize{
#'    \item ID: Assigned identification of the participant
#'    \item "Timezone_IANA": The IANA time zone identifier of the local
#'    or travel destination time zone (e.g., "America/New_York"
#'    or "Asia/Tokyo"). Users may use \code{\link{Geo2TZ}} or
#'    \code{\link{IANA}} to help filling this column.
#'    \item Date_Start: The date when the participant started to be in the
#'    local or travel destination.
#'    \item Date_End: The date when the participant ended being in the local
#'    or travel destination. Default to no input, which will uses the
#'    following start date of the destination.
#'    }
#'
#'    \item UTC: a data frame with columns
#'     \itemize{
#'      \item ID: As described above.
#'      \item UTC_Offset: The UTC offset of the local or travel destination
#'      time zone. Users may use \code{\link{Geo2TZ}} or \code{\link{IANA}}
#'      to help filling this column.
#'      \item Country_with_Daylight_Saving: Whether the local or travel
#'      destination practices daylight saving time (DST). Users may use
#'      \code{\link{Geo2TZ}}, \code{\link{TZwDST}} or \code{\link{UTCwDST}}
#'      when filling in the DST information.
#'      \item Date_Start: As described above.
#'      \item Date_End: As described above.
#'      }
#' }
#'
#'
#'
#' @seealso
#' \code{\link{Geo2TZ}} \code{\link{IANA}} \code{\link{TZwDST}}
#'
#'
#' @examples
#' Tlg <- TravelLog (Write = FALSE)
#'
#' print (Tlg)
#'
#'
#' @keywords Travel Log Template Timezone Shift
#'
#'
#' @export

TravelLog <- function (Type = "UTC", Write = FALSE, Dir = NULL) {

    Type <- toupper (Type)

    switch (
        Type,
        "TZ"  = {

            data <- data.frame (matrix (ncol = 4, nrow = 1))

            names (data) <- c (
                "ID", "IANA_TZ_Identifiers",
                "Date_Start", "Date_End"
            )

            data [1, ] <- c (
                "ExampleID", "America/New_York", "TRUE",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )
        },
        "UTC" = {
            data <- data.frame (matrix (ncol = 5, nrow = 1))

            names (data) <- c (
                "ID", "UTC_Offset", "Country_with_Daylight_Saving",
                "Date_Start", "Date_End"
            )

            data [1, ] <- c (
                "ExampleID", "+05:00", "TRUE",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

        },
        "Geo" = {

            data <- data.frame (matrix (ncol = 6, nrow = 2))

            names (data) <- c (
                "ID", "Municipal_or_Intermediate_Unit",
                "First_Order_Division",
                "Country",
                "Date_Start", "Date_End"
            )

            data [1, ] <- c (
                "ExampleID", "Île-de-France", "Metropolitan France",
                "France",
                as.character (Sys.Date () - 15),
                as.character (Sys.Date () - 10)
            )

            data [2, ] <- c (
                "ExampleID", "Paris","Ontario" ,"Canada",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )


            data [3, ] <- c (
                "ExampleID", "Montréal", "Québec" ,"Canada",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

            data [4, ] <- c (
                "ExampleID", "雲林縣","" ,"臺灣",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

            data [5, ] <- c (
                "ExampleID", "Buenos Aires",
                "Ciudad Autónoma de Buenos Aires" ,"Argentina",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

            data [6, ] <- c (
                "ExampleID", "Tesero",
                "Provincia di Trento" ,"Italia",
                as.character (Sys.Date ()),
                as.character (Sys.Date () + 1)
            )

        },
        stop ("Invalid Type. Please choose \"TZ\", \"UTC\" or
              \"Geo\".")
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
