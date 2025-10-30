# File ActiGlobe/R/GuessTZ.R
#
# Copyright (C) 2025  C. William Yao, PhD
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# @title GuessTZ
# @description Guess possible time zone based on the UTC offset
# @import parallel
# @param aOF A converted UTC offset in the POSIX format. `e.g., "aOF <- sprintf("%+03d00", OF)"`
# @param DT Only one date at a time
# @param iTZ The time zone when the recording started. When guessing time zone, it will prioritize matching to the initial geographic location even when the time change occurs. Default is "NULL". When specified as `"local"`, user's local time zone is assumed.
# @param All Logical, if TRUE, as default, it will provide all possible TZ codes. If FALSE, it will retrieve the first one.

GuessTZ = function(aOF, DT = NULL, iTZ = NULL, All = TRUE) {

  # Establish initial time zone...
  TZ1 = ifelse(iTZ == "local", Sys.timezone(), iTZ)

  if (is.null(iTZ)) TZ1 = NULL



  # Extract all known time zones
  oTZs <- OlsonNames()

  ## Process DT
  if (is.null(DT)) {

    # Determine if DST exists using time offset on January 1st of 2021
    DT <- as.POSIXct("2021-01-01", tz = "UTC")

  }

  if (!length(DT) == 1)  DT = DT[[1]]



  # Extract
  ## Check number of cores available...
  NCore = parallel::detectCores()

  if (NCore < 6) {
    # Step 1: Create a cluster
    cl <- parallel::makeCluster(4)  # Use all but one core

    # Step 2: Export variables and functions to cluster
    parallel::clusterExport(cl, varlist = c("DT"), envir = environment())

    # Step 3: Run the parallelized task
    Toffs <- parallel::parLapply(cl, oTZs, function(tz) {
      format(as.POSIXct(DT, tz = tz), "%z")
    })

    # Step 4: Clean up
    parallel::stopCluster(cl)

    # Optional: Convert result to character vector (like vapply would produce)
    Toffs <- unlist(Toffs)

  } else {

    Toffs <- vapply(oTZs,
                    function(tz) format(as.POSIXct(DT, tz = tz), "%z"),
                    character(1))

  }


  #### Step 1 Guess all possible TZ indicators
  pTZs <- sapply(aOF,
                 function(x) oTZs[Toffs %in% x])

  #### Step 2 Check if the initial time zone is included
  if (!is.null(TZ1))
    if (length(aOF) == 1) {

      pTZs <-ifelse(TZ1 %in% pTZs, TZ1, pTZs)

    } else {

      pTZs <- sapply(pTZs,
                     function(x) ifelse(TZ1 %in% x, TZ1, x))
    }


  #### Step 3 Keep only the first one if the All is set to FALSE
  if (!All)
    if (length(aOF) > 1){

      pTZs <- sapply(pTZs,
                     function(x) x[[1]])
    } else {
      pTZs = pTZs[[1]]
    }





  return(pTZs)

}

