#  File ActiGlobe/R/data.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2020-2025  C. William Yao, PhD
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#
#' @title IANA Time Table
#'
#' @description
#' A dictionary for standard abbreviation and labels of time-zone and their corresponding UTC offset.
#'
#'
#' @docType data
#' @keywords IANA
#' @name IANA
#' @usage data(IANA)
#' @format A simulated data with 100 rows and 5 variables:
#' \describe{
#' \item{Country_Code}{Age at the recuitment}
#' \item{TZ_Identifier}{Time zone identification in canonical format (i.e., continent/city), link format (aliases of the country or city) or  military-based label}
#' \item{Comment}{Additional detail about the TZ_Identifier}
#' \item{Type}{Indicating whether the `TZ_Identifier` is canonical or link format}
#' \item{UTC_Offset}{Time difference from the Universal Time Coordiante, similar to `Greenwich Mean Time (GMT)`}
#' \item{Time_Zone}{Standard time zone abbreviation}
#' \item{Source}{The different regions and files in the IANA time zone database. Note, backward mean the link time zones (aliases) points to canonical time zones for historical or compatibility reasons. Etcetera (Etc), is a catch-all category for time zones that don't fit neatly into other regions, including UTC and GMT. Factory is a special file that provides a default time zone setting for systems that don't have a specific time zone configured}
#' \item{Hour_Offset}{Numerated time difference from the Universal Time Coordiante, similar to `Greenwich Mean Time (GMT)`}
#' \item{Daylight_Saving}{A binary indicator indicating whether the indicator or label occurs during daylight saving time.}
#' }
#'
"IANA"





#' @title Travel East Sample Data
#'
#' @description
#' This sample data is a modified version of a continuous wrist-worn actigraphy recording (Philips `ActiWatch5`) collected over a month. The data was originally collected to monitor the influence of jetleg on professional athelets' performance during competitions.
#'
#' @docType data
#' @keywords FlyEast
#' @name FlyEast
#' @usage data(FlyEast)
#' @format A modified data with 40 thousands plus rows and 3 variables:
#' \describe{
#' \item{Activity}{The activity count measured by a tri-axis actigraphy}
#' \item{X2}{The light exposure data from the light sensor. All values are zero since the function was not disabled during the recording.}
#' \item{Marker}{A binary indicator generated when the wearer press the button to record onset of sleep or awake.}
#' }
#'
"FlyEast"




#' @title Adjusted Travel East Sample Data
#'
#' @description
#' This is the subset of the travel-adjusted `FlyEast` data. The data was originally collected to monitor the influence of jetleg on professional athelets' performance during competitions.
#'
#' @docType data
#' @keywords FlyEast_adj adjust
#' @name FlyEast_adj
#' @usage data(FlyEast_adj)
#' @format An adjusted FlyEast data with 40 thousands plus rows and 3 variables:
#' \describe{
#' \item{DateTime}{The time coordinates of each recorded activies in the standard date and time joint format}
#' \item{Date}{The date of each recorded activies}
#' \item{Hour}{The simplified time coordinates in hour form converted  from `DateTime`}
#' \item{Activity}{The activity count measured by a tri-axis actigraphy}
#' \item{Activity_ID}{Numeric values denoted the sequence of each activity count in the recording}
#' \item{Note}{Summary annotation derived from [BriefSum()].}
#' }
#'
#' @seealso [data(FlyEast)]
"FlyEast_adj"



#' @title Travel Log
#'
#' @description
#' A modified travel log documenting the dates when the wearer departs for long-distance travel.
#'
#' @docType data
#' @keywords travel, timezone, daylight_saving, diary
#' @name TLog
#' @usage data(TLog)
#' @format A data frame with over 40,000 rows and 5 variables:
#' \describe{
#'   \item{ID}{Pseudonym of the wearer.}
#'   \item{UTC_Offset}{UTC offset of the wearer's location or destination.}
#'   \item{Country_with_Daylight_Saving}{Binary indicator (TRUE/FALSE) for whether daylight saving time is observed at the location or destination.}
#'   \item{date_Start}{Start date of the wearer's initial location or date of departure.}
#'   \item{date_End}{Optional date when the wearer departs again from the initial location or previous destination.}
#' }
#' @seealso [TravelLog()], [IANA]
#'
"TLog"
