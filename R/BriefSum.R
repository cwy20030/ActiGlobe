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
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#' @importFrom lubridate dst
#' @importFrom lubridate ymd
#' @param df A data.frame containing the actigraphy records
#' @param SR The sampling rate of the actigraphy (unit at Hz). Note that Hz should be equal to or less than 1.
#' @param Start The starting date and time of the recording in formats like this "2021-03-05 18:31:03"
#' @param TZ The time zone when the recording started. (default = "local", which means user's local time zone)
#' @keywords Summary, Actigraphy
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


  # Define the last time point of the day ------------------
  #Subtractor = hms::as_hms(as.difftime(Epc, units = "secs"))

  ## Subtract Epoch converted time point from MN2
  #LstP <- MN2 - Subtractor
  #LstT = hms::as_hms(LstP)

  ## Separate Date and Time
  IniD = DateFormat(Start)

  IniT = gsub(IniD,"", Start) ### Remove date
  IniT = trimws(IniT)
  IniT = hms::as_hms(IniT)


  # Add Starting time of Recording -------------------------------
  ## Compute the number of data points recorded during the first day.
  Hr = 0
  if(!lubridate::hour(IniT)==0)  Hr = (nDPHr*floor(24 - lubridate::hour(IniT)))
  if(!lubridate::minute(IniT)==0|!lubridate::second(IniT)==0) Hr = Hr -1

  Mnt = 0
  if(!lubridate::minute(IniT)==0) Mnt = (nDPMn*(60-lubridate::minute(IniT)))
  if(!lubridate::second(IniT)==0) Mnt = Mnt -1

  Sec = 0
  if(!lubridate::second(IniT)==0) Sec = (nDPSc*(60 - lubridate::second(IniT)))



  Day1 = Hr + Mnt + Sec #### Remaining data points for the first day



  ## Compute the number of full day recordings --------------
  FDs = floor((DP - Day1)/(nDPHr*24))


  ## Compute the remaining datapoints needed to find out the end time on the last day ----------
  Rmn = DP - (Day1 + (FDs * nDPHr * 24) )


  ## Total number of days
  Y = ifelse(Rmn > 0, 1, 0)

  Days = FDs + 1 + Y

  ### All the Dates
  ADs = seq.Date(from = IniD, to = IniD + (Days-1), by = "day")



  # Compute End Time of Recording ---------------------
  nLstDP = Rmn


  #### Hours for the last day -----------------
  HrLst = floor(Rmn/nDPHr)

  Rmn = Rmn - HrLst*nDPHr

  #### Minutes for the last day
  if(!Rmn==0){
    MnLst = floor(Rmn/nDPMn)
    Rmn = Rmn - MnLst*nDPMn
  }


  #### Seconds for the last day
  if(!Rmn==0){
    SecLst = Rmn/nDPSc
  } else {
    SecLst = 0
  }


  if(HrLst<10) HrLst = paste0("0",HrLst)
  if(MnLst<10) MnLst = paste0("0",MnLst)
  if(SecLst<10) SecLst = paste0("0",SecLst)

  EndT = hms::as_hms(paste0(HrLst,":",MnLst,":",SecLst))




  # Define Time Zone ---------------
  UTCs = sapply(ADs, UTC, TZ)





  # Compute Number of Data Points ---------------------
  # Initialize Report ------------
  Summary = data.frame(ADs)
  names(Summary) = "Date"
  Summary$Epoch = Epc
  Summary$UTC = UTCs
  Summary$Daylight_Saving = lubridate::dst(lubridate::ymd(ADs, tz = TZ))
  Summary$Recording_Start = c(IniT, rep(MN, Days-1))
  Summary$Recording_End = c(rep(MN2, Days-1), EndT)

  #### Data Points and Summary
  nDP = c(Day1, rep(24 * nDPHr, FDs), nLstDP)
  Summary$nDataPoints = nDP
  Summary$Cumulative_Start_Second = cumsum((c(0, nDP[-length(nDP)])) * Epc) + Epc
  Summary$Cumulative_End_Second = cumsum(nDP * Epc)

  #### Set Exclusion and Warning
  Summary$Excluded = ifelse(!nDP  == 24 * nDPHr, TRUE, FALSE)
  Summary$Warning = ifelse(Summary$Excluded, "Incomplete Recording","")

  return(Summary)


}
