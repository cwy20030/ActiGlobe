#  File ActiGlobe/R/C2T.R
#
#  Copyright (C) 2025  C. William Yao, PhD
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

C2T <- function(Time){

  x <- suppressWarnings(as.numeric(as.character(Time)))

  if (length(na.omit(x)) == 0) {

    hms <- as.POSIXct(Time, format = TimeFormat(Time))

    decimal_hours <- as.numeric(format(hms, "%H")) +
      as.numeric(format(hms, "%M")) / 60 +
      as.numeric(format(hms, "%S")) / 3600


    x <- as.numeric(decimal_hours)
  }

  if (any(is.na(x))) warning(paste0("NAs introduced by coercion"))

  return(x)

}


### Potentially write a code to compute cosinor when both or neither the start or the end time falls are not the beginning/last time point of the recording.

