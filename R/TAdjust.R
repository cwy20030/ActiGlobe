#  File ActiGlobe/R/TAdjust.R
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
#' @title Adjust Time Shift based on Travel Log
#'
#' @description
#' `TAdjust()` is a function that corrects data points and time-shift based on traveling log.
#'
#'
#' @param Bdf A BriefSum object in data.frame
#' @param TLog A structured travel log containing date of travel and local time zone. Use `TravelLog()` to generate template.
#' @keywords Summary, Actigraphy
#' @seealso [TravelLog()]
#' @export


TAdjust = function(Bdf, TLog){

  ## Extract Essential Parameters
  LstP = max(Bdf$Cumulative_End_Second)
  a = Bdf$Cumulative_Start_Second
  b = Bdf$Cumulative_End_Second



 ## Convert Travel Log to Parameters ------------
  P = R2P(Bdf = Bdf,
          D = TLog$date_Start,
          U = TLog$UTC_Offset)


  Prd = P$Recording_Period
  U = P$UTC
  H2J = P$Hour_to_Adjust


  ### Add it back to the report ----------------
  Bdf$UTC = U
  Bdf$Recording_Period = Prd
  Bdf$Hour_to_Adjust = H2J





  ## Adjust DataPoint ------------------------
  P2J = H2J*3600

  ### Adjust Cumulative Start DataPoint ------------
  x = a - P2J
  x[x > LstP] = NA
  Bdf$Cumulative_Start_Second = x

  ### Adjust Cumulative End DataPoint ------------
  y = b - P2J
  y[y > LstP] = NA
  Bdf$Cumulative_End_Second = y

  ### Adjust Daily Data Point ------------
  N = y - x
  Bdf$nDataPoints = N

  ### Warning and Exclusion ------------
  ##### Remove Missing days
  Bdf$Warning[is.na(y)] = "Removed after adjustment"
  Bdf$Excluded[is.na(y)] = TRUE

  ##### Label Travel Days
  Bdf$Warning[Bdf$Date %in% TLog$date_Start] = "Travel Day"
  Bdf$Excluded[Bdf$Date %in% TLog$date_Start] = TRUE

  Bdf$Warning[Bdf$Date %in% (TLog$date_Start-1)] = "Day Before Travel"
  Bdf$Warning[Bdf$Date %in% (TLog$date_Start+1)] = "Day After Travel"

  ##### Label Incomplete
  fDP = max(Bdf$nDataPoints, na.rm = T)
  Bdf$Warning[Bdf$nDataPoints < fDP] = "Incomplete Recording"
  Bdf$Excluded[Bdf$nDataPoints < fDP] = TRUE



  ## Warning for time change due to daylight saving
  if (all(any(Bdf$Daylight_Saving), any(!Bdf$Daylight_Saving)))
    warning("Time change due to daylight saving occured at the local time zone.")


  return(Bdf)



}
