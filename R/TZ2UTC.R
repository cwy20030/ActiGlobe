#  File ActiGlobe/R/TZ2UTC.R
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
#' @title
#' Compute UTC offset based on Time Zone and Date
#'
#' @importFrom lubridate ymd_hms with_tz
#' @param DT The date in the format as "2021-03-05"
#' @param TZ The time zone when the recording started. (default = "local", which means user's local time zone)
#' @keywords Daylight Saving Timezone
#' @export
#' @examples
#'
#' x <- as.Date(c("2017-10-24","2017-11-20"))
#' TZ2UTC(DT = x, TZ = "America/Toronto") ## Expect two different UTCs
#'
#' \dontrun{
#' # A vector of dates
#' x =
#' seq.Date(from = as.Date("2017-10-24"),
#'            to = as.Date("2017-11-27"),
#'            by = "day")
#'
#' # If the user resides in a country that follows daylight saving time
#' TZ2UTC(DT = x, TZ = "local")
#'
#' # If not
#' # TZ2UTC(DT = x, TZ = "America/Toronto")
#' }
#'
#' @keywords UTC TZ
#' @export

TZ2UTC <- function(DT, TZ = "local") {

  TZ = ifelse(TZ == "local", Sys.timezone(), TZ)

  DT = as.Date(DT)
  x = lubridate::ymd_hms(paste0(DT," 12:00:00"), tz = TZ)
  y = lubridate::with_tz(x, tzone = "UTC")

  a = as.POSIXct(format(x, format = "%H:%M"), format = "%H:%M")
  b = as.POSIXct(format(y, format = "%H:%M"), format = "%H:%M")

  ab = a - b ### Compute time difference
  ab = as.numeric(ab, units = "hours") ### Convert to numeric


  ## Get the hours
  c = ifelse(ab <0, ceiling(ab), floor(ab)) ### Hour unit
  d2 = (abs(ab) - abs(c)) * 60   ### Minute unit

  mp = ifelse(ab <0, "-", "+") ### Check positive or negative

  e = ifelse(abs(c)<10, paste0(mp,"0",abs(c)), paste0(mp,c))
  f = ifelse(d2 == 0, "00", d2)


  Out = paste0("UTC", e,":",f)


    return(Out)
}
