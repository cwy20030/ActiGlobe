#  File ActiGlobe/R/UTC2Num.R
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
#
#
#' @title Convert Standard UTC Offset to Numbers
#'
#' @param x UTC Offsets <e.g., "UTC+09:30" or "UTC-07:00">
#' @export
#' @examples
#' # Convert UTC to numeric values
#' x = c("UTC+09:30","UTC-07:00")
#'
#' x1 = UTC2Num(x)
#'
#' print(x)
#'


UTC2Num = function(x) {

  x = gsub("UTC","",x)
  x = trimws(x) # Remove white space

  mp = unlist(sapply(x, function(i) {ifelse(grepl("-",i),-1, 1)}))
  a = gsub("\\+|\\-","",x) #### Remove plus minus sign

  b = as.numeric(gsub(":.*","",a))
  c = as.numeric(gsub(".*:","",a))/60

  x = mp * (b + c)

  return(x)
}
