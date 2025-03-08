#  File ActiGlobe/R/DateFormat.act.R
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
#' @title Detect Possible Datetime Format
#'
#' @description
#' This function will automatically detect possible date format. Users can choose to either reformat the date or report the datetime format detected.
#'
#'
#' @param DateTime A character string of date.
#' @param as.date A binary operator indicating whether to return a converted date based on the detection or the date time format. (default: TRUE)
#' @param Delim If the deliminator used to separate month and date is not of standard  (i.e., / or . or -), please specify it here. (default: NULL)
#' @return character
#' @export
#' @examples
#' # Example 1: When all dates have the same format
#' ## Create and store a date in a variable called DateTime
#' DateTime = c("2017/05/02", "2000/02/28", "1970/01/02")
#'
#' ## Ask DateFormat to tell us the format of the date.
#' print(DateFormat(DateTime))
#'
#' # Example 2: When multiple formats co-exist in a variable
#' ## Create and store a date in a variable called DateTime
#' DateTime = c("2017/05/02", "2000.Feb.28", "1970-11-02", "January 01, 2025", "December 12, 1980")
#'
#' ## Ask DateFormat to tell us the format of the date.
#' ### DO This!
#' #### Option 1.
#' print(lapply(DateTime,DateFormat))
#'
#' #### Option 2. To match the warning message to the items
#' for(x in DateTime){
#'
#' print(DateFormat(x))
#'
#' }
#'
#'
#' ### DO NOT!
#' print(DateFormat(DateTime))
#' ### Note, this process will fail because there are multiple formats
#'
#'
#'
#'


DateFormat <- function(DateTime, as.date = TRUE, Delim=NULL){



  fmts <- c("%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
            "%Y-%m", "%m-%Y",
            "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y",
            "%Y/%m", "%m/%Y",
            "%Y.%m.%d", "%m.%d.%Y",  "%d.%m.%Y",
            "%Y.%m", "%m.%Y")  # Add more formats as needed

  if(!is.null(Delim)){
    Base <- c("%Y-%m-%d", "%m-%d-%Y", "%d-%m-%Y",
              "%Y-%m", "%m-%Y")
    Base = gsub("-",Delim,Base) #Only one deliminator is allowed.
    fmts = c(fmts,Base)
  }

  formatedDate = as.Date(DateTime, format=fmts)

  Format = fmts[which(!is.na(formatedDate))] #Extract Possible format

  formatedDate = formatedDate[!is.na(formatedDate)] ## Extract the time format generated


  ## See which time format works for this
  if(length(Format)>1){
 Format = unlist(lapply(1:length(formatedDate),function(d){
          temp = as.character(formatedDate[d])
          temp = unlist(strsplit(temp,split="-"))

          if(all(as.numeric(temp)<1000)){
            NA
          } else {
            Format[d]
          }
          }))

 Format = Format[!is.na(Format)]
  }


# Post-process Check...
  if(length(Format)==0){
    warning(paste0("Possible illegal datetime format detected in ",DateTime,". Please, ensure that...
         1. the year is recorded as full four digits (i.e., 19xx).
         ==> Please, manually correct the year and try again.

         2. the deliminator used to separate month and date is not commonly used.
         ==> Please, specify the proper deliminator in Delim."))

    Format = ""
}

    if(length(Format)>1){
  warning("Unable to recognize the difference between date and month.
           Only the first detected format would be used.
            Please, manually set it using as.Date function if it is incorrect or convert the month number to name.")

    Format = Format[1]
    }


  if(as.date){
    return(as.Date(DateTime, format = Format))
  } else {
    return(Format)
  }



}

