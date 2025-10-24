#  File ActiGlobe/R/BriefSum.R
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
#' @title Summarize the Actigraphy Recording by Day
#' @import hms
#' @importFrom lubridate hour minute second dst ymd
#' @param df A data.frame containing the actigraphy records
#' @param SR The sampling rate of the actigraphy (unit at Hz). Note that Hz should be equal to or less than 1.
#' @param Start The starting date and time of the recording in formats like this "2021-03-05 18:31:03"
#' @param TZ The time zone when the recording started. (default = "local", which means user's local time zone)
#' @returns A data.frame where each row holds all metadata for one recording day: the calendar date, time--zone code, epoch length (seconds), UTC offset, daylight--saving flag, cumulative start/end seconds from midnight, recording start/end times (HH:MM:SS), any warning labels (e.g. `Travel`, `Incomplete Recording`), an exclusion flag, and the expected number of epochs for a full day
#' @keywords Summary Actigraphy
#' @examples
#' # Export the data in a list
#' \dontrun{
#' data(FlyEast)
#'
#' Bdf = BriefSum(df = FlyEast,
#'                SR = 1/60,
#'                Start = "2017-10-19 13:45:00")
#' print(Bdf)
#'
#' ## install library "zeallot"
#' ## library(zeallot)
#' ## c(Bdf, df) %<-%
#' ## BriefSum(df = FlyEast,
#' ##          SR = 1/60,
#' ##          Start = "2017-10-24 13:45:00")
#'
#' }
#'
#' @export

BriefSum = function(df, SR, Start, TZ = "local") {

  # Prepare Basic Variables ------------------
  MN = hms::as_hms("00:00:00")
  MN2 = hms::as_hms("24:00:00")
  TZ = ifelse(TZ == "local", Sys.timezone(), TZ)
  DP = nrow(df) # Number of Data Points
  Epc = 1/SR # Compute epoch length
  TT = DP * Epc # Total time of recordings in seconds

  nDPHr = 3600/Epc # Compute numbers of data points per hour
  nDPMn = 60/Epc  # Compute numbers of data points per minute
  nDPSc = 1/Epc  # Compute numbers of data points per second

  FDP = SR * 3600 * 24 # Total data points per 24 hours

  # Define the last time point of the day ------------------
  #Subtractor = hms::as_hms(as.difftime(Epc, units = "secs"))

  ## Subtract Epoch converted time point from MN2
  #LstP <- MN2 - Subtractor
  #LstT = hms::as_hms(LstP)

  ## Compute All time points for the recording.
  T = as.numeric(as.POSIXct(Start)) + (0:(nrow(df)-1) * Epc)

  ### Convert date time
  AllT = as.POSIXct(x = T, tz = TZ)

  ##### Extract date
  Ds = format(AllT,"%Y-%m-%d")
  ADs = unique(Ds)

  ##### Extract Time
  Ts = format(AllT, "%H:%M:%S")
  G = data.frame("Ds" = Ds, "Ts" = Ts)

  aTs = aggregate(Ts ~ Ds, data = G, FUN = min)
  names(aTs) = c("Date","Ini")
  aTs$End = aggregate(Ts ~ Ds, data = G, FUN = max)[[2]]

  ##### Daylight Saving
  DSTs = lubridate::dst(AllT)
  aDSTs = DST(ADs) ### Use negative to prioritize non-daylight saving time.
  ### Alternative code: ifelse(ADs %in% unique(Ds[!DSTs]), FALSE, TRUE)

  ###### Determine the influence of Daylight Saving
  GL = DST2GL(ADs)

  ##### Extract Time Zone
  TZ3 = format(AllT, "%Z")

  K = as.data.frame.matrix(table(TZ3, Ds))
  aTZs = as.data.frame(t(K))
  aTZs$TZ3 = names(aTZs)[[1]]
  ###### When daylight saving occurs, dynamically change UTCs based on the dominant TZ.
  if(length(aTZs)>1) aTZs$TZ3 = ifelse(aTZs[[1]] > aTZs[[2]], names(aTZs)[[1]], names(aTZs)[[2]])


  ###### Get and Convert TZ into UTC value
  TZs = format(AllT, "%z")
  UTCs <- paste0("UTC",
                 substr(TZs, 1, 1),
                 substr(TZs, 2, 3),
                 ":",
                 substr(TZs, 4, 5))

  B = as.data.frame.matrix(table(UTCs, Ds))
  aUTCs = as.data.frame(t(B))
  aUTCs$UTCs = names(aUTCs)[[1]]

  ###### When daylight saving occurs, dynamically change UTCs based on the dominant TZ.
  if(length(aUTCs)>1) aUTCs$UTCs = ifelse(aUTCs[[1]] > aUTCs[[2]], names(aUTCs)[[1]], names(aUTCs)[[2]])


  ###### DataPoints
  nDP = unlist(colSums(B))

  # Add the New Information Back to the df ---------------
  df$DateTime = AllT ### Date Time
  df$Date = Ds ### Date in string
  df$Time = Ts ### Time in string
  df$UTC = UTCs ### UTC in string
  df$DaylightSaving = DSTs ### Daylight saving indicator in logical
  df$nPoint = 1:nrow(df) ### cumulative data point index in numeric.



  # Initialize Report ------------
  Summary = data.frame(ADs)
  names(Summary) = "Date"
  Summary$Epoch = Epc
  Summary$UTC = aUTCs$UTCs
  Summary$TZ_code = aTZs$TZ3
  Summary$Daylight_Saving = aDSTs
  Summary$Recording_Start = aTs$Ini
  Summary$Recording_End = aTs$End
  Summary$GL_Offset = GL

  #### Data Points and Summary
  Summary$nDataPoints = nDP
  Summary$Cumulative_Start_Second = cumsum((c(0, nDP[-length(nDP)])) * Epc) + Epc
  Summary$Cumulative_End_Second = cumsum(nDP * Epc)

  #### Set Exclusion and Warning
  Summary$Excluded = ifelse(!nDP == 24 * nDPHr, TRUE, FALSE)
  Summary$Warning = ifelse(Summary$Excluded, "Incomplete Recording","")
  Summary$Warning =  ifelse(nDP > 24 * nDPHr, "Time Change", Summary$Warning)


  class(df) <- c("ActiGlobe","data.frame")
  class(Summary) <- c("ActiGlobe","data.frame")

  return(list("Bdf" = Summary, "df" = df))


}
