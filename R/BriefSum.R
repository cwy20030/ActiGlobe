#  File ActiGlobe/R/BriefSum.R
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
#' @title Summarize the Actigraphy Recording by Day
#'
#' @description
#' `BriefSum()` is a function that summarizes the actigraphy recording by day.
#' It generates a data.frame where each row holds all metadata for one recording
#' day: the calendar date, time--zone code, epoch length (seconds), UTC offset,
#' daylight--saving flag, cumulative start/end seconds from midnight, recording
#' start/end times (HH:MM:SS), any warning labels (e.g. `Travel`,
#' `Incomplete Recording`), an exclusion flag, and the expected number of
#' epochs for a full day.
#'
#' @importFrom lubridate hour minute second dst ymd
#'
#' @param df A data.frame of raw actigraphy recording. Both time and activity
#' count should be included in the \code{df}.
#' @param SR The sampling rate of the actigraphy (unit at Hz). Note that Hz
#' should be equal to or less than 1.
#' @param Start The starting date and time of the recording in the format
#' as "2021-03-05 18:31:03". See \code{\link[base]{as.POSIXct}} for more
#' details and  \code{\link{DateFormat}} and \code{\link{TimeFormat}} for
#' formatting help.
#' @param TZ The time zone when the recording started. (default = "local",
#' which means user's local time zone)
#'
#' @returns A named list with two elements:
#' \itemize{
#'   \item Bdf A data.frame containing brief summary information of each
#'   recording day. Columns include:
#'     \itemize{
#'     \item Date calendar date (YYYY-MM-DD)
#'     \item Epoch epoch length in seconds
#'     \item UTC dominant UTC offset string (e.g., "UTC+02:00")
#'     \item TZ_code time zone code (e.g., "EST")
#'     \item Daylight_Saving logical flag indicating DST for the day
#'     \item Recording_Start earliest recorded time for the day (HH:MM:SS)
#'     \item Recording_End latest recorded time for the day (HH:MM:SS)
#'     \item GL_Offset numeric offset returned by \code{\link{DST2GL}} for the
#'     day
#'     \item nDataPoints number of epochs observed for the day
#'     \item Cumulative_Start_Second cumulative start second from midnight
#'     for the day's first epoch
#'     \item Cumulative_End_Second cumulative end second from midnight for
#'     the day's last epoch
#'     \item Excluded logical flag; TRUE if the day is excluded (e.g.,
#'     incomplete)
#'     \item Warning character; warning label when applicable (e.g.,
#'     "Incomplete Recording", "Time Change")
#'     }
#'
#'   \item df The original input df augmented with additional columns (class
#'   ActiGlobe, data.frame):
#'     \itemize{
#'     \item DateTime POSIXct timestamp for each epoch (tz = TZ)
#'     \item Date date string (YYYY-MM-DD)
#'     \item Time time string (HH:MM:SS)
#'     \item UTC UTC offset string for each epoch
#'     \item DaylightSaving logical flag per epoch
#'     \item nPoint cumulative epoch index
#'     }
#' }
#' The function returns a list with both Bdf df = df) and sets classes
#' c("ActiGlobe","data.frame") on both returned data.frames.
#'
#' @examples
#'
#' # Import data
#' data (FlyEast)
#'
#' BdfList <- BriefSum (
#'     df = FlyEast,
#'     SR = 1 / 60,
#'     Start = "2017-10-19 13:45:00"
#' )
#'
#' str (BdfList)
#' # View(BdfList)
#'
#' ## install library "zeallot"
#' ## library(zeallot)
#' ## c(Bdf, df) %<-%
#' ## BriefSum(df = FlyEast,
#' ##          SR = 1/60,
#' ##          Start = "2017-10-24 13:45:00")
#'
#'
#' @keywords summary actigraphy
#' @export

BriefSum <- function (df, SR, Start, TZ = "local") {
    ## Checkpoint -----------------------
    if (!is.numeric (SR) || SR <= 0) {
        stop ("Sampling rate must be a positive numeric value")
    }

    # Prepare Basic Variables ------------------
    # MN <- hms::as_hms("00:00:00")
    # MN2 <- hms::as_hms("24:00:00")
    if (TZ == "local") TZ <- Sys.timezone ()
    Epc <- 1 / SR # Compute epoch length
    # DP <- nrow(df) # Number of Data Points
    # TT <- DP * Epc # Total time of recordings in seconds

    nDPHr <- 3600 / Epc # Compute numbers of data points per hour
    # nDPMn <- 60 / Epc # Compute numbers of data points per minute
    # nDPSc <- 1 / Epc # Compute numbers of data points per second

    # FDP <- SR * 3600 * 24 # Total data points per 24 hours

    # Define the last time point of the day ------------------
    # Subtractor = hms::as_hms(as.difftime(Epc, units = "secs"))

    ## Subtract Epoch converted time point from MN2
    # LstP <- MN2 - Subtractor
    # LstT = hms::as_hms(LstP)

    ## Compute All time points for the recording.
    Tm <- as.numeric (as.POSIXct (Start, tz = TZ)) +
        (0:(nrow (df) - 1) * Epc)
    ### Convert date time
    AllT <- if (is.numeric (Tm)) {
        as.POSIXct (x = Tm, origin = "1970-01-01", tz = TZ)
    } else {
        as.POSIXct (x = Tm, tz = TZ)
    }


    ##### Extract date
    Ds <- format (AllT, "%Y-%m-%d")
    ADs <- unique (Ds)

    ##### Extract Time
    Ts <- format (AllT, "%H:%M:%S")
    G <- data.frame ("Ds" = Ds, "Ts" = Ts)

    aTs <- aggregate (Ts ~ Ds, data = G, FUN = min)
    names (aTs) <- c ("Date", "Ini")
    aTs$End <- aggregate (Ts ~ Ds, data = G, FUN = max) [[2]]

    ##### Daylight Saving
    DSTs <- lubridate::dst (AllT)
    ### Use negative to prioritize non-daylight saving time.
    aDSTs <- DST (ADs, TZ = TZ)
    ### Alternative code: ifelse(ADs %in% unique(Ds[!DSTs]), FALSE, TRUE)

    ###### Determine the influence of Daylight Saving
    GL <- DST2GL (ADs, TZ = TZ)

    ##### Extract Time Zone
    TZ3 <- format (AllT, "%Z")

    K <- as.data.frame.matrix (table (TZ3, Ds))
    aTZs <- as.data.frame (t (K))
    aTZs$TZ3 <- names (aTZs) [[1]]
    ###### When daylight saving occurs, dynamically change UTCs based on the
    ###### dominant TZ.
    if (length (aTZs) > 1) {
        aTZs$TZ3 <- ifelse (aTZs [[1]] > aTZs [[2]],
            names (aTZs) [[1]], names (aTZs) [[2]]
        )
    }


    ###### Get and Convert TZ into UTC value
    TZs <- format (AllT, "%z")
    UTCs <- paste0 (
        "UTC",
        substr (TZs, 1, 1),
        substr (TZs, 2, 3),
        ":",
        substr (TZs, 4, 5)
    )

    B <- as.data.frame.matrix (table (UTCs, Ds))
    aUTCs <- as.data.frame (t (B))
    aUTCs$UTCs <- names (aUTCs) [[1]]

    ###### When daylight saving occurs, dynamically change UTCs based on the
    ###### dominant TZ.
    if (length (aUTCs) > 1) {
        aUTCs$UTCs <- ifelse (aUTCs [[1]] > aUTCs [[2]],
            names (aUTCs) [[1]], names (aUTCs) [[2]]
        )
    }


    ###### DataPoints
    nDP <- unlist (colSums (B))

    # Add the New Information Back to the df ---------------
    df$DateTime <- AllT ### Date Time
    df$Date <- Ds ### Date in string
    df$Time <- Ts ### Time in string
    df$UTC <- UTCs ### UTC in string
    df$DaylightSaving <- DSTs ### Daylight saving indicator in logical
    ### cumulative data point index in numeric.
    df$nPoint <-
        seq_len (length.out = nrow (df))


    # Initialize Report ------------
    Summary <- data.frame (ADs)
    names (Summary) <- "Date"
    Summary$Epoch <- Epc
    Summary$UTC <- aUTCs$UTCs
    Summary$TZ_code <- aTZs$TZ3
    Summary$Daylight_Saving <- aDSTs
    Summary$Recording_Start <- aTs$Ini
    Summary$Recording_End <- aTs$End
    Summary$GL_Offset <- GL

    #### Data Points and Summary
    Summary$nDataPoints <- nDP
    Summary$Cumulative_Start_Second <-
        cumsum ((c (0, nDP [-length (nDP)])) * Epc) + Epc
    Summary$Cumulative_End_Second <- cumsum (nDP * Epc)

    #### Set Exclusion and Warning
    Summary$Excluded <- ifelse (!nDP == 24 * nDPHr, TRUE, FALSE)
    Summary$Warning <- ifelse (Summary$Excluded, "Incomplete Recording", "")
    Summary$Warning <- ifelse (nDP > 24 * nDPHr, "Time Change",
                               Summary$Warning)


    class (df) <- c ("ActiGlobe", "data.frame")
    class (Summary) <- c ("ActiGlobe", "data.frame")

    return (list ("Bdf" = Summary, "df" = df))
}
