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
#' The function detects possible non-wearing periods within a longitudinal
#' recording. Currently, it supports only the Choi algorithm, which is a
#' threshold-based classifier using activity count.
#'
#'
#' @param data A data.frame of raw actigraphy recording. Both time and activity
#' count should be included in the \code{data}. See \code{VAct} and \code{VTm}
#' for further detail.
#'
#' @param VAct Optional character. Name of the activity column in \code{data}.
#'  If NULL, defaults to the second column of \code{data}.
#'
#' @param VTm Optional character. Name of the time or date.time index column in
#' \code{data}.If NULL, defaults to the first column of \code{data}.
#'
#' @param method Character string specifying detection method. Algorithm-
#' specific hyperparameters can be passedd through the \code{HyperP} argument.
#' Available methods include:
#' \itemize{
#'   \item "Choi": A modified Troiano algorithm designed to detect possible
#'   non-wear time based on time-dependent threshold of inactive period. This
#'   is currently the only supported method.
#' }
#'
#' @param HyperP A list of hyperparameters used to tune the non-wear detection
#' algorithm. The default values are set for the Choi algorithm, which is
#' currently the only supported method.
#' \itemize{
#'   \item "Choi": All hyperparameters require units in minutes.
#'    \itemize{
#'      \item ScreenEpoch: minimum threshold to classifying a segment as
#'      probable non-wear time; Default = 90 (minutes per epoch).
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
#'}
#'
#' @param Bdf Optional; a \code{\link{BriefSum}} object containing per-day
#' metadata for the recording. Note, if jet lag occurred during the recording,
#' please, update the metadata using \code{\link{TAdjust}} before passing to
#' this function.
#'
#' @param AddRatio Logical scalar. If `TRUE`, the function will calculate and
#'  add Percent_Nonwear to data. Default = FALSE
#'
#' @note
#' With Choi algorithm, the default resolution is \eqn{1/60}Hz (i.e., 1-minute
#' epoch). For recordings of higher resolution, \code{NonWear} will
#' automatically adjust the hyperparameters multiplicatively.
#'
#'
#' @returns
#' An amended raw recording data.frame with non-wearer detection results
#' storing in a new column:
#'   \code{pNonwear}, which stands for possible non-wear time. The values
#'   are logical, where \code{TRUE} indicates a possible non-wear period
#'   according to the specified method (e.g., Choi algorithm).
#'
#' When \code{Bdf} is provided, the function also returns an updated
#' \code{BriefSum} object with two additional columns:
#' \itemize{
#'   \item Possible_Nonwear_Time: total duration of possible non-wear time per
#'   day (in minutes).
#'   \item Percentage_Nonwear: a quality control indice based on the ratio
#'   of total wear time to possible nonwear time.
#'   }
#'
#'
#' @references
#' Choi L, Liu Z, Matthews CE, Buchowski MS. Validation of accelerometer wear
#' and nonwear time classification algorithm. Med Sci Sports Exerc. Feb 2011;
#' 43(2):357-64. doi:10.1249/MSS.0b013e3181ed61a3
#'
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
#'
#' @keywords cosinor
#' @export

NonWear <- function (data, VAct = NULL, VTm = NULL,
                     method = "Choi",
                     HyperP = list (ScreenEpoch = 90,
                                    ArtEpoch    = 2,
                                    TraceEpoch  = 30),
                     Bdf  = NULL,
                     AddRatio = FALSE) {

  # Check Point and Input Validation -------------------------
  ## Get Variable Names -------------
  if (is.null (VTm))  VTm  <- names (data) [[1]]
  if (is.null (VAct)) VAct <- names (data) [[2]]

  ## Validate Input -------------------
  activity <- ValInput (x = data [[VAct]], type = "Act")
  Tm       <- ValInput (x = data [[VTm]], type = "Time")


  ## Extract Additional Input ---------------------
  dt     <- diff (Tm)
  dt     <- dt [dt > 0]
  Epc    <- 1 / min (dt)
  SR     <- Epc / 3600
  Factor <- SR * 60 # Multiplicative Factor for the hyperparameters
  nPoint <- as.integer (round (24 * 3600 / Epc, 0))

  ## Check incomplete in time ------------------
  Index <- SeqFill (x      = Tm,
                    Step   = 1/Epc,
                    Format = "Index")

  nRep   <- ceiling (max (Index) / nPoint)
  tPoint <- as.integer (nPoint * nRep)

  if (!length (activity) == tPoint) {
    tSeq  <- seq_len (tPoint + 1)
    ToRm  <- tSeq [!tSeq %in% Index]
    TClr  <- TRUE
    y     <- rep (NA, tPoint)
    y [Index] <- activity

 } else {
   y <- activity
   TClr  <- FALSE
 }
  ## Detection ------------------
  Label  <- base::switch (method,
    "Choi" = Choi (y           = y,
                   ScreenEpoch = HyperP$ScreenEpoch * Factor,
                   ArtEpoch    = HyperP$ArtEpoch    * Factor,
                   TraceEpoch  = HyperP$TraceEpoch  * Factor),
    stop ("Invalid method specified. Please set to 'Choi'.")
  )

  ## Prepare Output -------------------------
  if (!is.null (Bdf) | AddRatio){
    GID <- rep (seq_len (nRep), each = nPoint)
    nNW <- tapply (Label, GID, function (x)
      sum (as.integer (x), na.rm = TRUE))
    pNW <-  100 * nNW / nPoint
  }

  if (TClr)  Label <- Label [-ToRm]
  data$pNonWear <- Label

  if (AddRatio) {
    QC.Idx   <- rep (pNW, each = nPoint)

    if (TClr)
      QC.Idx <- QC.Idx [-ToRm]
    data$pNW <- QC.Idx
  }


  # Return -------------------------
  if (is.null (Bdf)){

    return (data)

  } else {

    Bdf$Possible_Nonwear_Time <- nNW
    Bdf$Percentage_Nonwear    <- pNW

    return (list (data = data, Bdf = Bdf))
  }

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
#'
#' @param y Numeric vector of observed activity counts. Typically represents
#'   activity levels measured over time.
#'
#' @param ScreenEpoch \code{frame} hyperparameter for the Choi algorithm,
#' representing the minimum length of non-wear time to be detected (in minutes).
#' Default is 90 minutes.
#'
#' @param ArtEpoch \code{allowanceFrame} hyperparameter for the Choi algorithm,
#' representing the maximum length of activity allowed within a non-wear period
#' (in minutes). Default is 2 minutes.
#'
#' @param TraceEpoch \code{streamFrame} hyperparameter for the Choi algorithm,
#' representing the interval of the upstream and downstream windows adjacent to
#' the \code{ArtEpoch}. This aims to check for activity around potential
#' non-wear periods (in minutes). Default is 30 minutes.
#'
#'
#' @returns
#' A logical vector of the same length as \code{x}, where \code{TRUE} indicates
#' a possible non-wear period according to the Choi algorithm.
#'
#'
#' @references
#' Choi L, Beck C, Liu Z, Moore R, Matthews C, Buchowski M (2021).
#' _PhysicalActivity: Process Accelerometer Data for Physical Activity
#' Measurement_. doi:10.32614/CRAN.package.PhysicalActivity
#'
#'
#' @noRd

Choi <- function (y,
                  ScreenEpoch = 90,
                  ArtEpoch    = 2,
                  TraceEpoch  = 30) {
  # Check Point and Input Validation -------------------------
    if (is.null (TraceEpoch) || is.na (TraceEpoch)) {
      TraceEpoch <- round (0.5 * ScreenEpoch)
    }

    Mark.na <- is.na (y)
    y       <- ifelse (Mark.na, 0, y)
    ln      <- length (y)

    # Classification -------------------------
    ## Step 0. Parameter Extraction (Note 1) ---------------------------------
    y.Pos  <- ifelse (y > 0, 1, 0)

    ### Segment the recording into consecutive series of zeros and non-zeros
    ### (active)
    Seg     <- rle (y.Pos)
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
    y.Act    <- rep (Seg$ActWear, as.integer (Segln))
    ActSeg   <- rle (y.Act)
    ### Classify non-wear time based on the gap between active segments
    ActSeg$LongInAct <- !ActSeg$values & ActSeg$lengths >= ScreenEpoch


    # Prepare Output -------------------------
    Out <- rep (ActSeg$LongInAct, as.integer (ActSeg$lengths))

    return (Out)
}
