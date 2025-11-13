#  File ActiGlobe/R/Num2UTC.R
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
#
#
#' @title Convert Numbers to Standard UTC Offset
#' @param x UTC Offsets <e.g., "UTC+09:30" or "UTC-07:00">
#' @export
#' @seealso \code{\link{UTC2Num}}
#' @examples
#' # Convert UTC to numeric values
#' x = c(9.5,-7)
#'
#' x1 = Num2UTC(x)
#'
#' print(x)
#'


Num2UTC = function(x) {

  ## Get the hours
  A = ifelse(x <0, ceiling(x), floor(x)) ### Hour unit
  B = (abs(x) - abs(A)) * 60   ### Minute unit


  mp = ifelse(x <0, "-", "+") ### Check positive or negative

  C = ifelse(abs(A)<10, paste0(mp,"0",abs(A)), paste0(mp,A))
  D = ifelse(B == 0, "00", B)


  Out = paste0("UTC", C,":",D)


  return(Out)
}
