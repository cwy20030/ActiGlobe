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
