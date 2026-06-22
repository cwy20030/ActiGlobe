#  File ActiGlobe/R/data.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2020-2025  C. William Yao, PhD
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
#
#' @title IANA Time Table
#'
#' @description
#' A dictionary for standard abbreviation and labels of time-zone and their
#' corresponding UTC offset.
#'
#'
#' @docType data
#'
#' @keywords IANA
#'
#' @name IANA
#'
#' @usage data(IANA)
#'
#' @format A modified IANA 2025b database
#'  \describe{
#'  \item{Country_Name}{Full name of the country (e.g., "Canada").}
#'  \item{Country_Code}{Two-letter ISO 3166-1 alpha-2 country code (e.g., "CA").
#'  }
#'  \item{Time_Zone_IANA}{Canonical IANA timezone identifier (e.g.,
#'  "America/Montreal").}
#'  \item{TZ_Identifiers}{Alternative or historical identifiers linked
#'  to the IANA timezone.}
#'  \item{Standard_TZ_Long_Name}{Human-readable name of the timezone
#'  during standard time, without daylight saving adjustment (e.g., "Eastern
#'  Standard Time").}
#'  \item{Standard_TZ_Code}{Abbreviation used during standard
#'  (non-daylight saving) time only (e.g., "EST").}
#'  \item{Standard_Offset}{UTC offset in clock time during standard time,
#'  excluding daylight saving adjustment (e.g., +03:00).}
#'  \item{Standard_Offset_Numeric}{Numeric UTC offset in hours during standard
#'  time (e.g., 3.00).}
#'  \item{Observes_DST}{Logical flag indicating whether the timezone observes}
#'  \item{DST_TZ_Long_Name}{Human-readable name of the timezone
#'  during daylight saving time or other recurrent practice of time shift
#'  (e.g., "Australian Central Daylight Time").}
#'  \item{DST_TZ_Code}{Abbreviation used during daylight saving time or other
#'  recurrent practice of time shift only (e.g., "ACDT").}
#'  \item{DST_Offset}{UTC offset in clock time during daylight saving or
#'  recurrent time change (e.g., +10:30).}
#'  \item{DST_Offset_Numeric}{Numeric UTC offset in hours during daylight
#'  saving or recurrent time change (e.g., 10.5)}
#'  }
#'
#'
#' @seealso \code{\link{TZwDST}} \code{\link{MapTZ}}
"IANA"


#' @title Travel East Sample Data
#'
#' @description
#' This sample data is a modified version of a continuous wrist-worn actigraphy
#'  recording (Philips `ActiWatch5`) collected over a month. The data was
#'  originally collected to monitor the influence of jetleg on professional
#'  athelets' performance during competitions.
#'
#'
#' @docType data
#'
#' @keywords FlyEast
#'
#' @name FlyEast
#'
#' @usage data(FlyEast)
#'
#' @format A modified data with 40 thousands plus rows and 3 variables:
#' \describe{
#' \item{Activity}{The activity count measured by a tri-axis actigraphy}
#' \item{X2}{The light exposure data from the light sensor. All values are zero
#'  since the function was not disabled during the recording.}
#' \item{Marker}{A binary indicator generated when the wearer press the button
#'  to record onset of sleep or awake.}
#' }
#'
#' @seealso
#' \code{\link{BriefSum}} \code{\link{TAdjust}} \code{\link{TravelLog}}
#' \code{\link{IANA}}
"FlyEast"


#' @title Adjusted Travel East Sample Data
#'
#' @description
#' This is the subset of the travel-adjusted `FlyEast` data. The data was
#' originally collected to monitor the influence of jetleg on professional
#' athelets' performance during competitions.
#'
#' @docType data
#'
#' @keywords FlyEast_adj adjust
#'
#' @name FlyEast_adj
#'
#' @usage data(FlyEast_adj)
#'
#' @format An adjusted FlyEast data with 40 thousands plus rows and 6 variables:
#' \describe{
#' \item{DateTime}{The time coordinates of each recorded activies in the
#' standard date and time joint format}
#' \item{Date}{The date of each recorded activies}
#' \item{Hour}{The simplified time coordinates in hour form converted  from
#' `DateTime`}
#' \item{Activity}{The activity count measured by a tri-axis actigraphy}
#' \item{Activity_ID}{Numeric values denoted the sequence of each activity
#' count in the recording}
#' \item{Note}{Summary annotation derived from \code{\link{BriefSum}}.}
#' }
#'
#'
#' @seealso \code{\link{BriefSum}} \code{\link{TAdjust}} \code{\link{FlyEast}}
"FlyEast_adj"


#' @title Travel Log
#'
#' @description
#' A modified travel log documenting the dates when the wearer departs for
#' long-distance travel.
#'
#' @docType data
#'
#' @keywords travel timezone daylight saving diary
#'
#' @name TLog
#'
#' @usage data(TLog)
#'
#' @format A data frame with over 40,000 rows and 5 variables:
#' \describe{
#'   \item{ID}{Pseudonym of the wearer.}
#'   \item{UTC_Offset}{UTC offset of the wearer's location or destination.}
#'   \item{Country_with_Daylight_Saving}{Binary indicator (TRUE/FALSE) for
#'   whether daylight saving time is observed at the location or destination.}
#'   \item{Date_Start}{Start date of the wearer's initial location or date of
#'    departure.}
#'   \item{Date_End}{Optional date when the wearer departs again from the
#'   initial location or previous destination.}
#' }
#'
#'
#' @seealso \code{\link{TAdjust}} \code{\link{TravelLog}} \code{\link{IANA}}
"TLog"
