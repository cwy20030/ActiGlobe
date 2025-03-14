#  File ActiGlobe/R/R2P.R
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
#' @title
#' Convert Traveling Log to Parameters for TAdjust
#' @param Bdf A BriefSum object
#' @param D The date traveling by plane.
#' @param U The UTC offset values <e.g., "UTC+09:30" or "UTC-07:00">
#' @export
#'



R2P = function(Bdf, D, U){


  # D = c("2025-04-26", "2025-07-26")
  # U = c("UTC+09:30", "UTC-07:00")


  ## Extract Date info from summary
  Dates = Bdf$Date ## All Recording Dates
  MaxDate = max(Dates) ## Last Date
  MinDate = min(Dates) ## First Date


  ## Check if UTC is in the Bdf
  if (!"UTC" %in% names(Bdf)) stop("Bdf must be an object created by BriefSum.")
  U1 = UTC2Num(Bdf$UTC[[1]])


  ## Extract UTC offset
  if (any(grepl("UTC", U))) U = UTC2Num(U)


  #### Double check for date coherence ---------
  D = as.Date(D)


  # Process UTC and Time adjustment -------------

  Bdf$UTC.old = Bdf$UTC[[1]]

 for (d in 1:length(D)){

  if (D[d] %in% Dates) { ### Check if the travel dates in the log existed in recording

    if (d < length(D)){

      Period = as.Date(D[d]:(D[d+1]-1))

    } else {

      Period = as.Date(D[d]:MaxDate)
    }

    Bdf$Recording_Period[Dates %in% Period] = d
    Bdf$UTC[Dates %in% Period] = Num2UTC(U[d])
   }
  }

  ### Compute Changes in Hours -------------
  Bdf$Hour_to_Adjust = UTC2Num(Bdf$UTC) - UTC2Num(Bdf$UTC.old)



  ## Update Recording Period
  if (!MinDate %in% D)
    Bdf$Recording_Period = ifelse( is.na(Bdf$Recording_Period), 1,  Bdf$Recording_Period + 1)




  # Output --------
  Out = Bdf[c("Date","Recording_Period","UTC","Hour_to_Adjust")]
  return(Out)

}
