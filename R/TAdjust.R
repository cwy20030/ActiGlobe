#  File ActiGlobe/R/TAdjust.R
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
#' @title Adjust Time Shift based on Travel Log
#'
#' @description
#' `TAdjust()` is a function that corrects data points and time-shift based on travelling log. Note that it is important to ensure that the UTC-offset value is correct. When in doubt, please use the UTC function or consult IANA table in the package. Daylight saving will not be reassessed as in BriefSum function.
#' @param Bdf A \code{\link{BriefSum}} object containing per-day metadata for the recording.
#' @param TLog A structured travel log containing date of travel and local time zone. Use `TravelLog()` to generate template.
#' @param TZ The time zone when the recording started. (default = "NULL", which will disregard the use of the initial geographical location-based time zone indicator)
#' @param fork Logical, if TRUE, it will use parallel processing to speed up the computation. Default is FALSE.

#' @return A \code{\link{BriefSum}} object with adjusted data points and time shift based on travel log.
#'
#' @examples
#' \dontrun{
#'
#' # Import sample data
#' data (FlyEast)
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <- BriefSum (
#'     df = FlyEast,
#'     SR = 1 / 60,
#'     Start = "2017-10-19 13:45:00"
#' )
#'
#' # Extract only the summary report
#' Bdf <- BdfList$Bdf
#'
#' # Import sample travel Log
#' data (TLog)
#'
#' # Adjust time shift based on travel log
#' Bdf.adj <- TAdjust (Bdf, TLog)
#'
#' # Display the summary
#' View (Bdf)
#' View (Bdf.adj) ### Focus on the dates after 2017-11-01
#' }
#'
#' @keywords Adjust Actigraphy
#' @seealso \code{\link{TravelLog}}
#' @export


TAdjust <- function (Bdf, TLog, TZ = NULL, fork = FALSE) {

    ## Extract Essential Parameters ----------------
    DT <- Bdf$Date

    Epc <- Bdf$Epoch
    SR <- 1 / Epc
    FDP <- SR * 3600 * 24 # Total data points per 24 hours

    UTCs <- Bdf$UTC
    aDST <- Bdf$Daylight_Saving
    RS <- Bdf$Recording_Start
    RE <- Bdf$Recording_End
    GL0 <- Bdf$GL_Offset
    nDP <- Bdf$nDataPoints
    a <- Bdf$Cumulative_Start_Second
    b <- Bdf$Cumulative_End_Second
    LstP <- max (b)
    Exc <- Bdf$Excluded
    Wrn <- Bdf$Warning

    ### aTZ-------------------
    IANAi <- get0("IANAi", envir = asNamespace("ActiGlobe")) # Time zone database
    iTZ <- IANAi$Timezone_IANA
    STD <- IANAi$TZ_Code

    aTZ <- sapply (Bdf$TZ_code, function (x) { # Time zone identifier per day
        iTZ [STD %in% x] [1]
    })


    ## Convert Travel Log to Parameters ------------

    D <- DateFormat (TLog$date_Start)
    if (any (!D %in% DT)) {

        D2k <- which (D %in% DT)
        TLog <- TLog [D2k, ]
        D <- DateFormat (TLog$date_Start)

    }

    P <- R2P (
        Bdf = Bdf,
        D = D,
        U = TLog$UTC_Offset
    )


    Prd <- P$Recording_Period
    U <- P$UTC
    aH2J <- P$Hour_to_Adjust

    ################## Adjust for Daylight Saving Changes #####################

    ### Guess if the UTC may experience Time Change due to daylight saving calender
    pUDST <- UTCwDST (U)
    SameUTC <- UTCs == U ### Determine if the original UTC from BriefSum is the same as the new UTC from R2P
    nuUDST <- ifelse (SameUTC, aDST, pUDST) ### New logical indicator of whether daylight saving occurs...


    ### If TZ is not specified, guess it.

    gTZ <- sapply (1:length (DT), function (x) {

        GuessTZ (
            aOF = sprintf ("%+03d00", UTC2Num (U [[x]])),
            DT = DT [[x]],
            iTZ = TZ,
            All = FALSE,
            fork = fork
        )
    })


    ### Step 1 Change NDPs
    #### Based on the new daylight saving, we will change the NDPs and cumulative time...
    A1 <- ifelse (nuUDST & GL0 == 0, 0, -1 * GL0) ### Adjusting factor for incorrect initial GL guesses
    GL <- GL0 + A1 ### Update GL....

    NDP <- ifelse (A1 == 0, nDP, nDP + (A1 * 3600 / Epc)) ### Remove the inappropriately adjusted gain or loss due to suspected time shift
    mDP <- max (cumsum (nDP)) - max (cumsum (NDP)) ### Number of data points needed to be added.
    NDP [length (NDP)] <- NDP [length (NDP)] + mDP ### Add the remainant to the last day. Leave the question whether it is more than one fDP to later step.


    ### Step 2 Edit cumulative time based on NDPs
    a1 <- cumsum ((c (1, NDP [-length (NDP)])) * Epc) # Line 134 from  BriefSum
    b1 <- cumsum (NDP * Epc) # Line 135 from  BriefSum


    ################## Adjust based on Traveling #####################

    ### If U from R2P is the same as the original then H2J to 0
    H2J <- ifelse (SameUTC, 0, aH2J)


    ## Adjust DataPoint ------------------------
    P2J <- H2J * 3600


    ### Adjust Cumulative Start DataPoint ------------
    x <- a1 + P2J
    x [x > LstP] <- NA


    ### Adjust Cumulative End DataPoint ------------
    y <- b1 + P2J
    y [y > LstP] <- NA


    ### Adjust Cumulative Start DataPoint V2 will give you the same as the above------------
    #  if ( Version == 2) {
    #    T2A = c(0,diff(H2J))
    #    x = a1
    #    y = b1
    #    STR = which(!T2A == 0)
    #    for (nr in STR) {
    #      x[nr:length(x)] = x[nr:length(x)] + T2A[[nr]]*3600
    #      y[nr:length(y)] = y[nr:length(y)] + T2A[[nr]]*3600
    #    }
    #  }


    ### Adjust Daily Data Point ------------
    Sec <- y - x
    N <- 1 + (Sec / Epc) #### Plus one for the End


    ################### Check the Last Day ###################
    Idxl <- length (DT)
    Nl <- as.integer (N [Idxl]) #### Last number of data
    Dl <- DT [Idxl] #### Last date of the recording
    Tl <- as.numeric (as.POSIXct (Dl, tz = gTZ [Idxl])) ### The starting second of the last day in number

    Epl <- as.numeric (Epc [[1]])

    Timel <- seq (
        from = Tl,
        by = Epl,
        length.out = Nl
    ) #### All time points on the last day.


    HMSl <- as.POSIXct (Timel, tz = gTZ [Idxl])
    Datel <- suppressWarnings (DateFormat (HMSl)) ### Extract all the dates
    uniDl <- unique (Datel) #### Check the numbers of the unique date spanning for the last day.

    TPl <- max (which (Datel %in% Dl)) ### Find the last time point of the last day.
    REl <- HMSl [TPl]
    RE [Idxl] <- format (REl, "%H:%M:%S") ### Last Time point

    ##### In the rare event when there is more data points on the last day than allowed....
    if (length (uniDl) > 1) {

        Nl2 <- Nl - FDP [Idxl] ### Additional Date Points to be relocated


        #### The OCD double check flow control...
        if (Nl2 > 0) {
            N [Idxl] <- FDP [Idxl] ### Update the total data point on the last day

            yMax <- y [Idxl] ### Extract the Max cumulative end second for the recording

            y [Idxl] <- y [Idxl] - Nl2 * Epc [Idxl] #### Update the cumulative end second on the last day

            #### New Last Day
            DT [Idxl + 1] <- as.character (as.Date (DT [Idxl]) + 1)
            Epc [Idxl + 1] <- Epc [Idxl] ### Assume the same epoch length as the last
            U [Idxl + 1] <- U [Idxl] ### Assume the same UTC as the last
            gTZ [Idxl + 1] <- gTZ [Idxl] ### Assume the same TZ as the last
            nuUDST [Idxl + 1] <- DST (
                DT = DT [Idxl + 1],
                TZ = gTZ [Idxl]
            ) ### Check if DST occur
            RS [Idxl + 1] <- RS [Idxl] ### Assume the same begining time as the last
            RE [Idxl + 1] <- format (HMSl [length (HMSl)], "%H:%M:%S") ### Extract the New last time of recording
            GL [Idxl + 1] <- DST2GL (DT = as.POSIXct (DT [Idxl + 1], tz = gTZ [Idxl])) ### Check GL due to DST
            N [Idxl + 1] <- Nl2 ### Cumulative data points on the NEW last day
            x [Idxl + 1] <- y [Idxl] + Epc [Idxl] ### Cumulative start second on the NEW last day
            y [Idxl + 1] <- yMax ### Cumulative end second on the NEW last day
            Prd [Idxl + 1] <- Prd [Idxl] ### Assume the same recording period as the last
            H2J [Idxl + 1] <- H2J [Idxl] ### Assume the same hour to adjust as the last

        }


    }


    # Add it back to the report ----------------
    ##  Initialize Report ------------
    Summary <- data.frame (matrix (nrow = length (DT), ncol = length (names (Bdf))))
    names (Summary) <- names (Bdf)
    Summary$Date <- DT
    Summary$Epoch <- Epc
    Summary$UTC <- U
    Summary$TZ_code <- gTZ
    Summary$Daylight_Saving <- nuUDST
    Summary$Recording_Start <- RS
    Summary$Recording_End <- RE
    Summary$GL_Offset <- GL

    #### Data Points and Summary
    Summary$nDataPoints <- N
    Summary$Cumulative_Start_Second <- x
    Summary$Cumulative_End_Second <- y


    ### Warning and Exclusion ------------
    ##### Initiation
    Summary$Warning <- ""
    Summary$Excluded <- FALSE

    ##### Remove Missing days
    Summary$Warning [is.na (y)] <- "Removed after adjustment"
    Summary$Excluded [is.na (y)] <- TRUE

    ##### Label Travel Days
    Summary$Warning [Summary$Date %in% D] <- "Travel Day"
    Summary$Excluded [Summary$Date %in% D] <- TRUE

    D <- as.Date (D)

    Summary$Warning [Summary$Date %in% (D - 1)] <- "Day Before Travel"

    D2 <- D [grep ("Travel", Bdf$Warning [Bdf$Date %in% D])] ### To ensure only adding a proper label to the day after travelling.
    Summary$Warning [Summary$Date %in% (D2 + 1)] <- "Day After Travel"


    ##### Label Incomplete
    if (length(FDP) < length(Summary$nDataPoints)) FDP <- rep(FDP[[1]],length(Summary$nDataPoints))
    Summary$Warning [Summary$nDataPoints < FDP] <- "Incomplete Recording"
    Summary$Excluded [Summary$nDataPoints < FDP] <- TRUE

    if (length (uniDl) > 1) {
        Summary$Warning [Idxl] <- "Original Last Day" ### Update the warning on the last day
        Summary$Excluded [Idxl] <- FALSE ### Update the exclusion on the last day
    }

    Summary$Recording_Period <- Prd
    Summary$Hour_Adjusted <- H2J

    ## Warning for time change due to daylight saving
    if (any (nuUDST)) {
        message ("
Time change due to daylight saving occured at the local time zone.")
    }


    class (Summary) <- c ("ActiGlobe", "data.frame")
    return (Summary)


}
