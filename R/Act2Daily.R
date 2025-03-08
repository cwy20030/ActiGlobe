#  File ActiGlobe/R/Act2Daily.R
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
#' @title
#' Extract Actigraphy Daily Recording
#' @param df A data.frame containing the actigraphy records
#' @param Bdf A BriefSum object (default NULL, i.e., not included). This is essential when time adjustment is needed.
#' @param ID The subject's ID which would be used to create a folder.
#' @param TUnit The unit of time to be displayed in the daily actigraphic recording exported. These include: "day" "hour" "minute" "second"  (default: "hour")
#' @param VAct The name of the variable containing activity score  (defult assumed the second variable)
#' @param VTm The name of the variable containing timestamp (default assumed the first variable)
#' @param Incomplete A binary operator to decide whether to keep any less than 24hr-long recordings. (default: FALSE)
#' @keywords Daily Actigraphy Separate
#' @export



Act2Daily = function(df, Bdf, ID, TUnit = "hour", VAct = NULL, VTm = NULL, Incomplete=FALSE){


  # Determine time divider based on TUnit ----------------
  TDivider = ifelse(TUnit == "day", 3600*24,
                    ifelse(TUnit == "hour", 3600,
                           ifelse(TUnit == "minute", 60,
                                  ifelse(TUnit == "second", 1, NA))))


  # Prompt for valid TUnit if NA
  if (is.na(TDivider)) {
    TUnit = Demand(c("day", "hour", "minute", "second"), "Time Unit")
    TDivider = ifelse(TUnit == "day", 3600*24,
                      ifelse(TUnit == "hour", 3600,
                             ifelse(TUnit == "minute", 60,
                                    ifelse(TUnit == "second", 1, NA))))
  }

  ## VAct -----------
  if (is.null(VAct)) VAct = names(df)[[2]]
  if (is.null(VTm)) VTm = names(df)[[1]]



  ## Get Warning and Exclusion Criteria ---------------
  W = Bdf$Warning
  Ecl = Bdf$Excluded
  fDP = max(Bdf$nDataPoints, na.rm = TRUE) ### Full day datapoint

  #### Keep incomplete recording if specified.
  if (Incomplete) Ecl[W == "Incomplete Recording"] = FALSE


  ## Check if Bdf exist -----------------------
  Date = Bdf$Date
  Epc = unique(Bdf$Epoch)
  Ini = Bdf$Cumulative_Start_Second
  End = Bdf$Cumulative_End_Second


  ## Prepare for Datalist ------------
  Out = rep(list(list()), length(Date))
  names(Out) = Date

  #### Process ------------------

  for (d in 1:length(Date)) {

    if (!Ecl) {

      D = Date[[d]]
      Sbt = min(Ini[[d]]) - Epc
      S = Ini[[d]]/Epc #### Starting data point
      E = End[[d]]/Epc #### Ending data point
      Time = seq(Ini[[d]], End[[d]], by = Epc)   #### All the time


      ### Initialize Temp to collect daily activity
      Temp = data.frame(Time)
      Temp$Time = (Time - Sbt)/TDivider ##### Convert Time to Specified Unit
      Temp$Act = df[S:E, VAct]    #### Activity Score
      names(Temp) = c(VTm, VAct)


      Out[[D]] = Temp

    }

  }



  return(Out)

}
