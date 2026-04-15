#  File ActiGlobe/R/NonWear.R
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
#' @title Detection for Possible Non-wearer Periods
#'
#' @description
#' `NonWear()` is a function that detects possible non-wearing periods within
#' a longitudinal recording. Currently, it supports only the Choi algorithm and
#' require \eqn{1/60}Hz sampling rate.
#'
#' @param data A data.frame of raw actigraphy recording. Both time and activity
#' count should be included in the \code{data}. See \code{VAct} and \code{VTm}
#' for further detail.
#' @param VAct Optional character. Name of the activity column in \code{data}.
#'  If NULL, defaults to the second column of \code{data}.
#' @param VTm Optional character. Name of the date.time index column in
#' \code{data}.If NULL, defaults to the first column of \code{data}.
#' @param method Character string specifying detection method
#' \itemize{
#'   \item "Choi": A modified Troiano algorithm designed to detect possible
#'   non-wear time using moving window approach. The algorithm considers the
#'   following hyperparameters:
#'    \itemize{
#'      \item ScreenEpoch: minimum threshold to classifying a segment as
#'      probable non-wear time; Default = 90
#'      (minutes per epoch).
#'      \item Artifact Control:
#'       \itemize{
#'        \item ArtEpoch: The maximum interval of spontaneous activity
#'        (artifact) allowed between two possible non-wear time periods.
#'        Default = 2 (minutes)
#'        \item TraceEpoch: The minimum time of inactivity for the upstream and
#'        downstream of the identified spontaneous activity to be considered
#'        the entire segment as possible non-wear. Default = 30 (minutes).
#'        }
#'        }
#' }
#' @param HyperP a list of hyperparameters used to tune the non-wear detection
#' algorithm. The default values are set for the Choi algorithm, which is
#' currently the only supported method.
#'
#' @returns
#' An amended data.frame with non-wearer detection results storing in a new
#' column:
#'   \item \code{pNonWear}, which stands for possible non-wear time. The values
#'   are logical, where \code{TRUE} indicates a possible non-wear period
#'   according to the specified method (e.g., Choi algorithm).
#'
#' @references
#' Choi L, Liu Z, Matthews CE, Buchowski MS. Validation of accelerometer wear
#' and nonwear time classification algorithm. Med Sci Sports Exerc. Feb 2011;
#' 43(2):357-64. doi:10.1249/MSS.0b013e3181ed61a3
#'
#' @seealso
#' \code{\link[PhysicalActivity]{wearingMarking}}
#'
#'
#' @examples
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
#'   BriefSum (
#'   data = FlyEast,
#'   SR = 1 / 60,
#'   Start = "2017-10-24 13:45:00"
#'   )
#'
#' data <- BdfList$data
#'
#' # Detect possible non-wear time using the Choi algorithm with default
#' # hyperparameters (i.e., ScreenEpoch = 90, ArtEpoch = 2, TraceEpoch = 30)
#' data <- NonWear (data = data,
#'                  VAct = "Activity",
#'                  VTm = "Time",
#'                  method = "Choi")
#'
#'
#' # Visualize the detected non-wear time (red) against raw data (black)
#' ## First create a new variable
#' plot (y = data$Activity, x = data$DateTime, type = "l")
#' points (y = data$pNonWear * 1000,
#'         x = data$DateTime,
#'         col = "red", pch = 16, cex = 0.5)
#'
#' @keywords cosinor
#' @export

NonWear <- function (data, VAct = NULL, VTm = NULL,
                      method = "Choi",
                      HyperP = list (ScreenEpoch = 90,
                                     ArtEpoch    = 2,
                                     TraceEpoch  = 30)
                      ) {

  # Check Point and Input Validation -------------------------
  ## Get Variable Names -------------
  if (is.null (VTm))  VTm  <- names (data) [[1]]
  if (is.null (VAct)) VAct <- names (data) [[2]]

  ## Validate Input -------------------
  activity <- ValInput (x = data [[VAct]], type = "Act")
  time     <- ValInput (x = data [[VTm]], type = "Tm")

  ## Extract Additional Input ---------------------
  dt     <- diff (time)
  dt     <- dt [dt > 0]
  Epc    <- 1 / min (dt)
  SR     <- Epc / 3600
  Factor <- SR * 60 # Multiplicative Factor for the hyperparameters

  base::switch (method,
    "Choi" = {
      Label  <- Choi (x           = activity,
                      ScreenEpoch = HyperP$ScreenEpoch * Factor,
                      ArtEpoch    = HyperP$ArtEpoch    * Factor,
                      TraceEpoch  = HyperP$TraceEpoch  * Factor,
                      newcolname  = "wearing"
                      )
    },
    stop ("Invalid method specified. Please set to 'Choi'.")
  )

  data$pNonWear <- Label
  return (data)

}




# Pre-defined helper functions -------------------------------
#' @title Choi Algorithm for Non-wear Detection
#'
#' @description The function was modified from the PhysicalActivity package's
#' marking function. Assumptions were inherited based on the original code.
#' Based on the original implementation \code{\link[PhysicalActivity]{marking}},
#' the algorithm is a two-step classification system. See detail for more
#' information. In contrast to the original package, the function relies on
#' run length encoding \code{rle} algorithm to perform step-wise classification.
#'
#' @details
#' Code Assumptions:
#' - filling NAs in activity count with zeros
#' - default sampling rate at \eqn{1/60}Hz
#' - the non-wear time is defined by the gap between wearing sessions
#'
#' Algorithm Steps:
#' \itemize{
#'  \item Identify periods of activity via negative exclusion criteria (i.e.,
#'  mini-epochs of activity that are less than \code{ArtEpoch} minutes long and
#'  are surrounded by at least \code{TraceEpoch} minutes of inactivity on both
#'  sides).
#'  \item Classify periods of non-wear events (gap) based on time between the
#'  identified active periods (i.e., periods of inactivity that are at least
#'  \code{ScreenEpoch} minutes long).
#' }
#'
#' @param x A numeric vector of activity counts.
#' @param ScreenEpoch \code{frame} hyperparameter for the Choi algorithm,
#' representing the minimum length of non-wear time to be detected (in minutes).
#' Default is 90 minutes.
#' @param ArtEpoch \code{allowanceFrame} hyperparameter for the Choi algorithm,
#' representing the maximum length of activity allowed within a non-wear period
#' (in minutes). Default is 2 minutes.
#' @param TraceEpoch \code{streamFrame} hyperparameter for the Choi algorithm,
#' representing the interval of the upstream and downstream windows adjacent to
#' the \code{ArtEpoch}. This aims to check for activity around potential
#' non-wear periods (in minutes). Default is 30 minutes.
#'
#' @returns
#' A logical vector of the same length as \code{x}, where \code{TRUE} indicates
#' a possible non-wear period according to the Choi algorithm.
#'
#' @references
#' Choi L, Beck C, Liu Z, Moore R, Matthews C, Buchowski M (2021).
#' _PhysicalActivity: Process Accelerometer Data for Physical Activity
#' Measurement_. doi:10.32614/CRAN.package.PhysicalActivity
#'
#' @noRd

Choi <- function (x,
                  ScreenEpoch = 90,
                  ArtEpoch    = 2,
                  TraceEpoch  = 30,
                  newcolname  = "pNonWear") {
  # Check Point and Input Validation -------------------------
    if (is.null (TraceEpoch) || is.na (TraceEpoch)) {
      TraceEpoch <- round (0.5 * ScreenEpoch)
    }

    Mark.na <- is.na (x)
    x       <- ifelse (Mark.na, 0, x)
    ln      <- length (x)

    # Classification -------------------------
    ## Step 0. Parameter Extraction (Note 1) ---------------------------------
    x.Pos  <- ifelse (x > 0, 1, 0)

    ### Segment the recording into consecutive series of zeros and non-zeros
    ### (active)
    Seg     <- rle (x.Pos)
    Segln   <- Seg$lengths
    #### Epoch length
    Segln.us <- c (TraceEpoch + 1, Segln [-length (Segln)]) # adjacent upstream
    Segln.ds <- c (Segln [-1], TraceEpoch + 1) # adjacent downstream


    ### Extract start and end positions of each segment
    Seg$End <- cumsum (Segln)
    Seg$Ini <- c(1,(Seg$End [-length (Segln)] + 1))

    ## Step 1. Check criteria for mini-epoch of activity
    Seg$Active      <- Seg$values == 1
    Seg$mini.Active <- Seg$Active & Segln <= ArtEpoch

    ## Step 2. Check if adjacent zero segment length meets the threshold
    Seg$us.Zero  <- Seg$mini.Active & (Segln.us > TraceEpoch)
    Seg$ds.Zero  <- Seg$mini.Active & (Segln.ds > TraceEpoch)

    ## Step 3. Classify possible active time and non-wear time
    Seg$pArt     <- Seg$us.Zero &  Seg$ds.Zero
    Seg$ActWear  <- Seg$Active  & !Seg$pArt


    ## Step 4. Classify non-wearing time
    x.Act    <- rep (Seg$ActWear, Segln)
    ActSeg   <- rle (x.Act)
    ### Classify non-wear time based on the gap between active segments
    ActSeg$LongInAct <- !ActSeg$values & ActSeg$lengths >= ScreenEpoch

    # Prepare Output -------------------------
    Out <- rep (ActSeg$LongInAct, ActSeg$lengths)

    return (Out)
}
