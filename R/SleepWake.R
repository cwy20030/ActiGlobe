#  File ActiGlobe/R/SleepWake.R
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
#' @title Classification of Possible Sleep Wake Periods
#'
#' @description
#' A wrapper function for classifying possible sleep and wake periods based on
#' activity count. Please, read the \strong{Details} section below for more
#' information on the implemented algorithms and their hyperparameters.
#'
#' @details
#' The following information contains brief description on the sleep-wake
#' classification algorithms implemented in this function. Currently supported
#' methods include: "CircaCP", "CK" (i.e., "Cole–Kripke"), and "Okley". For
#' more details, please refer to the original publications and the references
#' provided below.
#'
#' \strong{CircaCP Algorithm}:
#'
#' This algorithm is a rhythm-based unsupervised classifier. The algorithm
#' first transforms each daily recording using a nonlinear least square cosinor
#' model. The fitted model is then used to guide the identification of the
#' change point (CP) in the activity pattern. Optimal CP is determined using
#' the modified information criteria and the Calinski–Harabasz criterion.
#' By default, recordings with a third or more missing values (e.g., non-wear
#' time or unrecorded time) will be excluded from classification. The algorithm
#' supports epoch lengths of 15, 30, or 60 seconds. Users may tune the
#' threshold for classification by changing the default value of
#' \code{CutPoint} and the distribution of in the \code{HyperP} argument.
#'
#' \strong{CK (Cole-Kripke) Algorithm}:
#'
#' This threshold-based supervised classifier detects the possible sleep wake
#' periods based on sleep index. The index is computed using weighted sum of
#' rescaled activity counts around a centered window. Assumed weights were
#' derived by Cole and colleagues using an in-house model selection software.
#' The algorithm supports epoch lengths of 10, 30, or 60 seconds. Users may
#' tune the threshold for classification by changing the default value of
#' \code{CutPoint} in the \code{HyperP} argument.
#'
#' \strong{Okley Algorithm}:
#'
#' This threshold-based supervised classifier detects possible sleep wake
#' periods based on the weighted sum of activity counts around a centered
#' window. The algorithm supports epoch lengths of 15, 30, 60, or 120 seconds.
#' Users may tune the threshold for classification by changing the default
#' value of \code{CutPoint} in the \code{HyperP} argument.
#'
#'
#' @param data A data.frame of raw actigraphy recording. Both time and activity
#' count should be included in the \code{data}. See \code{VAct} and \code{VTm}
#' for further detail.
#'
#' @param VAct Optional character. Name of the activity column in \code{data}.
#'  If NULL, defaults to the second column of \code{data}.
#'
#' @param VDT Optional character. Name of the \code{POSIXct} datetime column in
#' \code{data}.If NULL, defaults to "DateTime" of \code{data}.
#'
#' @param method Character string specifying detection method. For details on
#' the available methods and their hyperparameters, see
#' \itemize{
#'   \item "CircaCP": A local change-point detection algorithm guided by
#'   cosinor rhythm, proposed by Chen and Sun (2024).
#'
#'   \item "CK": A threshold-based classifier proposed by Cole and Kripke
#'   (1992). The algorithm dichotomies each epoch based on a composite score
#'   (a sum of weighted activity counts).
#'
#'   \item "Okley": A threshold-based classifier proposed by Oakley (1997). The
#'   algorithm is the primary sleep-wake scoring method used in the Actiware
#'   software by Respironics, Inc.
#' }
#'
#' @param HyperP Optional; a list of hyperparameters used to tune the sleep
#' wake detection algorithms. The specific hyperparameters depend on the chosen
#' method. See details below.
#' \itemize{
#'   \item "CK":
#'    \itemize{
#'      \item CutPoint: Numeric scalar. The threshold for
#'      classifying sleep vs. wake. Default = 1
#'      \item Pad: Character scalar. Padding method for edge epochs where the
#'      full window overhangs. Available methods include:
#'        \itemize{
#'        \item NA (default, no padding): edge epochs return NA (i.e.,
#'        non-classified)
#'        \item Reflect: pad by reflecting the data at the edges (e.g., y[2],
#'        y[3], for left edge)
#'        \item Zero: pad with zeros at the edges
#'        }
#'        }
#'
#'   \item "CircaCP":
#'    \itemize{
#'      \item Dist: The assumed distribution (family in regression) of the
#'      activity counts within a given day. Supported types of distribution
#'      includes:
#'        \itemize{
#'          \item `Exponential` Exponential distribution
#'          \item `Gamma` Gamma distribution
#'          \item `Gaussian` Normal distribution
#'          \item `Poisson` Poisson distribution
#'          \item `ZAG` Zero-Augmented Gamma distribution (Default)
#'        }
#'      \item CutPoint: Numeric scalar between \eqn{[0, 1]}. The
#'      threshold is used to dichotomize the fitted cosinor curve into active
#'      vs. inactive periods by CircaCP. Default = 0.2.
#'      \item CutpNW: Numeric scalar of the maximum percentage of non-wear
#'      time allowed for a day to be classified. Default = 33.33 (i.e., one
#'      third of the day).
#'      \item NDays: Numeric scalar. Minimum numbers of consecutive
#'      days for classification. Default = 5. Note that for serial processing,
#'      the function will automatically adjust the threshold down-to two days
#'      at the minimum.
#'      \item ScreenEpoch: Numeric scalar. The minimum threshold to classifying
#'      a segment as probable non-wear time; Default = 120 (minutes per epoch).
#'      Note, any changes to \code{ScreenEpoch} will be passed onto both
#'      \code{\link{NonWear}} and \code{\link[CircaCP]{screen_wear}}.
#'      \item ArtEpoch: Numeric scalar. The maximum interval of spontaneous
#'      activity (artifact) allowed between two possible non-wear time periods.
#'      Default = 2 (minutes)
#'      \item TraceEpoch: Numeric scalar. The minimum time of inactivity for
#'      the upstream and downstream of the identified spontaneous activity to
#'      be considered the entire segment as possible non-wear. Default = 30
#'      (minutes).
#'      \item Pad: Character scalar. See \code{Pad} in "CK" method above
#'      for details.
#'      }
#'
#'   \item "Oakley":
#'    \itemize{
#'      \item Pad: Character scalar. See \code{Pad} in "CK" method above
#'      for details.
#'      \item CutPoint: Optional; numeric scalar OR the string "automatic".
#'      Default = "automatic".
#'       \itemize{
#'        \item "automatic": Actiware Auto Wake Threshold computed from data.
#'        \item Numeric: User defined cutoff value. Note, a numeric value
#'        should not be wrapped by quotes (i.e., 40 but not \code{"40"}).
#'        }
#'      \item Scaler: Optional, a numeric scaler for tuning automatic
#'      threshold. It has no effect on user-defined cutoff value. Default is
#'      0.88888 (per Actiware manual).
#'        }
#' }
#'
#' @param Simple Optional; logical. If `TRUE`, the function returns a vector of
#' labels from sleep-wake classification results. Note that **NO** additional
#' computation (e.g., total sleep time) will be performed, either. If `FALSE`
#' (default), classification results are amended to the input data.
#'
#' @param Bdf Optional; a \code{\link{BriefSum}} object containing per-day
#' metadata for the recording. Note, if jet lag occurred during the recording,
#' please, update the metadata using \code{\link{TAdjust}} before passing to
#' this function.
#'
#'
#' @returns
#' An amended raw recording data.frame with non-wearer detection results
#' storing in a new column:
#'   \code{pSleep_Wake}, which stands for possible sleep or wake label. The
#'   values are categorical, where
#'   \itemize{
#'        \item "S": indicates that the wearer was possibly asleep
#'        \item "W": indicates that the wearer was possibly awake
#'        \item NA: indicates non-classified epochs due to algorithm
#'        constraints (e.g., rejected due to insufficient recording by
#'        \code{\link[CircaCP]{screen_wear}}).
#'        }
#'
#' When \code{Bdf} is provided, the function also returns an updated
#' \code{BriefSum} object with two additional columns:
#' \itemize{
#'   \item Total_Sleep_Time: total duration of possible sleep time per
#'   day (in minutes).
#'   \item Wake_Sleep_Ratio: time spent awake-to-sleep-ratio in percentage.
#'   }
#'
#' @references
#' Chen S, Sun X. Validating CircaCP: a generic sleep–wake cycle detection
#' algorithm for unlabelled actigraphy data. Royal Society Open Science. 2024;
#' 11(5)doi:10.1098/rsos.231468
#'
#' Cole RJ, Kripke DF, Gruen W, Mullaney DJ, Gillin JC. Automatic Sleep/Wake
#' Identification From Wrist Activity. Sleep. 1992;15(5):461-469.
#' doi:10.1093/sleep/15.5.461
#'
#' Oakley NR. Validation with polysomnography of the Sleepwatch sleep/wake
#' scoring algorithm used by the Actiwatch activity monitoring system. Mini
#' Mitter Co. Sleep. 1997;2:1-140.
#'
#' @seealso
#' \code{\link[CircaCP]{sleep_detection}}
#'
#'
#' @examples
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
#'   BriefSum (
#'   data  = FlyEast,
#'   SR    = 1 / 60,
#'   Start = "2017-10-24 13:45:00"
#'   )
#'
#' data <- BdfList$data
#'
#' # Apply Cole-Kripke's sleep wake classification
#' SleepWake (data = data, method = "CK", VAct = "Activity",
#' HyperP = list (CutPoint = 1))
#'
#' # Apply Chen's CircaCP sleep wake classification
#' SleepWake (data = data, method = "CircaCP", VAct = "Activity",
#' HyperP = list (CutPoint = 0.2))
#'
#' # Apply Oakley's sleep wake classification
#' SleepWake (data = data, method = "Oakley", VAct = "Activity",
#' HyperP = list (CutPoint = "automatic"))
#'
#'
#' @keywords sleep
#' @export


SleepWake <- function (data, VAct = NULL, VDT = "DateTime",
                       method = "CK",
                       HyperP = list (CutPoint    = NULL,
                                      Pad         = NA,
                                      Scaler      = 0.88888,
                                      Dist        = "ZAG",
                                      CutpNW      = 33.33,
                                      NDays       = 5L,
                                      ScreenEpoch = 120L,
                                      ArtEpoch    = 2L,
                                      TraceEpoch  = 30L),
                       Simple = FALSE,
                       Bdf    = NULL) {

  # Step 0. Check Point and Input Validation -------------------------
  if (!method %in% c("CK", "CircaCP", "Oakley")) {
    stop ("`method` must be one of 'CK', 'CircaCP', or 'Oakley'.")
  }

  ## Get Variable Names -------------
  if (is.null (VAct)) VAct <- names (data) [[2L]]

  ## Validate Input -------------------
  activity <- ValInput (x = data [[VAct]], type = "Act")
  DT       <- ValInput (x = data [[VDT]], type = "DT", SplitDT = TRUE)
  Ds       <- DT$Date
  Tm       <- DT$Time
  Hr       <- DT$Hour

  ## Extract Additional Input ---------------------
  dt     <- diff (Hr)
  dt     <- dt [dt > 0L]
  Epc    <- 1L / min (dt)
  SR     <- Epc / 3600L
  Factor <- SR * 60L # Multiplicative Factor for the hyperparameters

  Label <- switch (method,

                   "CK" = {
                     if (is.null (HyperP$CutPoint))
                       HyperP$CutPoint = 1L

                     ColeKripke (y        = activity,
                                 Epc      = Epc,
                                 CutPoint = HyperP$CutPoint)
                   },

                   "CircaCP" = {
                     if (is.null (HyperP$CutPoint))
                       HyperP$CutPoint = 0.2

                     df <- data.frame (Time     = Tm,
                                       Activity = activity,
                                       Date     = Ds,
                                       Hour     = Hr)

                     eCircaCP (data        = df,
                               Epc         = Epc,
                               CutPoint    = HyperP$CutPoint,
                               Dist        = HyperP$Dist,
                               CutpNW      = HyperP$CutpNW,
                               NDays       = HyperP$NDays,
                               ScreenEpoch = HyperP$ScreenEpoch,
                               ArtEpoch    = HyperP$ArtEpoch,
                               TraceEpoch  = HyperP$TraceEpoch)

                   },

                   "Oakley" = {
                     if (is.null (HyperP$CutPoint))
                       HyperP$CutPoint = "automatic"

                     Oakley (y        = activity,
                             Epc      = Epc,
                             CutPoint = HyperP$CutPoint,
                             Pad      = HyperP$Pad,
                             Scaler   = HyperP$Scaler)

                   }
  )

  if (Simple) {
    return (Label)
  } else {

    data$pSleep_Wake <- Label

    Gps <- rle (as.numeric (as.Date(Ds)))

    if (!is.null (Bdf)) {
      nSW <- tapply (Label, as.factor (Ds), function (x)
        sum (as.integer (x), na.rm = TRUE))
      pWS <-  100 * (1 -  (nSW / Gps$lengths))

      Bdf$Total_Sleep_Time <- nSW / (360L / Epc) # in hours
      Bdf$Wake_Sleep_Ratio <- pWS # in percentage

      return (list (data = data, Bdf = Bdf) )
    } else {
      return (data)
    }

  }


}




# Pre-defined helper functions -------------------------------

#' @title Cole-Kripke Algorithm for Sleep Wake Detection
#'
#' @description
#' The Cole-Kripke algorithm is a threshold-based classifier. It uses rolling-
#' window to compute composite scores based on weighted activity counts.
#'
#'
#' @details
#' There are three steps in the algorithm:
#'
#' Rescale Counts:
#' The algorithm starts from
#'
#' Algorithm Steps:
#' Step 1. Rescale: divide activity counts \code{y} by 100 and capping at 300.
#' \deqn{y^{\prime} \in [0,300]}
#'
#' Step 2. Compute a weighted sum (Sleep Index, SI) over a 7-epoch window of
#' rescaled counts, with specific weights for each epoch position relative to
#' the current epoch.
#'
#' \deqn{SI = 0.001 * (W4      * y^{\prime}_{-4} +
#'                     W3      * y^{\prime}_{-3} +
#'                     W2      * y^{\prime}_{-2} +
#'                     W1      * y^{\prime}_{-1} +
#'                     W0      * y^{\prime}_{0}  +
#'                     W_{-1}  * y^{\prime}_{+1} +
#'                     W_{-2}  * y^{\prime}_{+2})
#'                     }
#'
#' @param y Numeric vector of observed activity counts. Typically represents
#'   activity levels measured over time.
#'
#' @param Epc Numeric scalar. The epoch length in seconds (e.g., 60 for
#' 1-minute epochs).
#'
#' @param CutPoint Numeric scalar. The threshold for classifying sleep vs. wake.
#' Default = 1.
#'
#' @param Pad Character scalar. Padding method for edge epochs where the full
#' window overhangs. Available methods include:
#' \itemize{
#'  \item NA (default, no padding): edge epochs return NA
#'  \item Reflect: pad by reflecting the data at the edges (e.g., y[2], y[3],
#'  ... for left edge)
#'  \item Zero: pad with zeros at the edges
#' }
#'
#' @returns
#' A logical vector of the same length as \code{x}, where \code{TRUE} indicates
#' a possible non-wear period according to the Choi algorithm.
#'
#'
#' @references
#' Cole RJ, Kripke DF, Gruen W, Mullaney DJ, Gillin JC. Sleep, 15(5):461–469,
#' 1992. doi:10.1093/sleep/15.5.461
#'
#'
#' @noRd

ColeKripke <- function (y, Epc, CutPoint = 1L, Pad = NA){

  # Step 0: Check Point and Parameter Extraction ---------------
  Params <- switch (
    as.character (round (Epc)),
    "60" = c (106L, 54L,  58L,  76L,  230L,  74L,  67L),
    "30" = c (50L,  30L,  14L,  28L,  121L,  8L,   50L),
    "10" = c (550L, 378L, 413L, 699L, 1736L, 287L, 309L),
    stop ("Cole-Kripke algorithm is only defined for sampling rate at
          0.0167 Hz (60s epochs), 0.0333 Hz (30s epochs),
          or 0.1 Hz (10s epochs).")
  )

  Params <- setNames (Params, c("W4", "W3", "W2", "W1", "W0", "W_1", "W_2"))

  Len <- length (y)

  # Step 1: Rescale and cap counts -----------------
  y     <- pmin (y / 100, 300)
  pads  <- Padding (x = y, Window = c(4L, 2L), Pad = Pad)
  lpads <- pads [seq_len (4L)]
  rpads <- pads [c (4L + Len + c (1L, 2L))]
  # Step 2: Weighted sum over 7-epoch window  ---------------
  # (lag 4..1, current, lead 1..2)
  Pick <- function (i) {
    if (i > 0)      c (lpads [seq_len (i)], y [1L : (Len - i)])  # to left
    else if (i < 0) c (y [(1L - i) : Len], rpads [seq_len (-i)]) # to right
    else y
  }

  SI.CK <- 0.01 * (Params ["W4"]  * Pick (4L)  +
                   Params ["W3"]  * Pick (3L)  +
                   Params ["W2"]  * Pick (2L)  +
                   Params ["W1"]  * Pick (1L)  +
                   Params ["W0"]  * Pick (0L)  +
                   Params ["W_1"] * Pick (-1L) +
                   Params ["W_2"] * Pick (-2L))

  # Step 3: Classify -----------
  Out <- ifelse (SI.CK < CutPoint, "S", "W")


  return (Out)
}






#' @title Execution for CircaCP Sleep-Wake Scoring
#'
#' @import CircaCP
#'
#'
#' @param data A data.frame of raw actigraphy recording. Both date, time and
#' activity count should be included in the \code{data}.
#'
#' @param Epc Integer. Epoch length should be in 60 seconds.
#'
#' @param CutPoint CutPoint: Optional; numeric scalar between \eqn{[0, 1]}.
#' The threshold is used to dichotomize the fitted cosinor curve into active
#' vs. inactive periods by CircaCP. Default = 0.2.
#'
#' @param Dist Character scalar. The assumed distribution (family in
#' regression).
#'
#' @param CutpNW: Numeric scalar of the maximum percentage of non-wear
#' time allowed for a day to be classified. Default = 33.33 (i.e., one
#' third of the day).
#'
#' @param NDays: Numeric scaler. Minimum numbers of consecutive
#' days for classification.
#'
#' @param ScreenEpoch: Numeric scaler. Note, any changes to
#' \code{ScreenEpoch} will be passed onto both \code{\link{NonWear}} and
#' \code{\link[CircaCP]{screen_wear}}.
#'
#' @param ArtEpoch: Numeric scaler.
#'
#' @param TraceEpoch: Numeric scaler.
#'
#'
#' @return Integer vector (same length as y):
#'   S = sleep, W = wake, NA = edge epoch (insufficient window).
#'
#' @seealso
#' \code{\link[CircaCP]{screen_wear}}  \code{\link{NonWear}}
#'
#'
#' @references
#' Actiwatch Communication and Sleep Analysis Software instruction manual
#' (Respironics, Inc.)
#'
#' https://github.com/ghammad/pyActigraphy
#'
#' @examples
#' # Create quick summary of the recording with adjustment for daylight saving.
#' SleepWeake (data = FlyEast_adj,
#'             VAct = Activity,
#'             VDT = "DateTime",
#'             method = "CircaCP")
#'
#'
#' @noRd

eCircaCP <- function (data, Epc, CutPoint = 0.2, Dist = "ZAG", CutpNW = 33.33,
                      NDays = 5L, ScreenEpoch = 120L, ArtEpoch = 2L,
                      TraceEpoch = 30L) {
  # Step 0: Check Point and Parameter Extraction ---------------
  ### CutPoint
  if (CutPoint < 0 || CutPoint > 1) {
    stop ("`CutPoint` must be a numeric scalar between 0 and 1.")
  }

  ### Epc
  if (!round (Epc) %in% c (15L, 30L, 60L)) {
    stop (
      "Chen's CircaCP algorithm algorithm is not defined for epoch length ",
      Epc, " seconds. Accepted values: 15, 30, and 60. See pacakge `gsignal`."
      )
  }

  # Convert Date
  Ds <- as.Date (data$Date, format = "%m/%d/%Y")
  ds <- paste0 (as.integer (format (Ds, "%m")), "/",
                as.integer (format (Ds, "%d")), "/",
                format (Ds, "%y"))
  data$Date <- ds

  # Step 1. Remove excessive non-wear recording --------
  Temp <- NonWear (data        = data,
                   VAct        = "Activity",
                   VTm         = "Hour",
                   AddRatio    = TRUE,
                   HyperP      = list (ScreenEpoch = ScreenEpoch,
                                       ArtEpoch    = ArtEpoch,
                                       TraceEpoch  = TraceEpoch))

  Gps      <- rle (as.numeric (Ds))
  Temp$Day <- rep (as.integer (Gps$values),
                   as.integer (Gps$lengths))
  sbData   <- subset (Temp, subset = !Temp$pNW > CutpNW)

  ## Flow control minimum days of recordings
  tDs      <- length (unique (sbData$Day))

  if (tDs < 2L)
    stop ("Insufficient number of days in recording (minimum requirement = ",
      NDays," days). Please, check the recording and try again.")

  if (tDs < NDays) {
    Insufficient <- TRUE
    N2           <- ceiling (tDs / 2)
    warning (
      "Insufficient number of days in recording (minimum requirement = ",
      NDays," days). Minimum required days is readjusted to ",N2," days.")

    NDays <- N2
  } else {
    Insufficient <- FALSE
  }

  # Step 2. Prepare data for CP detection ----------
  sbData$id <- "JDoe" # Placeholder ID, replace with actual ID if available"
  sbData    <- sbData [c ("id", "Date", "Time", "Activity", "Day")]

  # Step 3. Run CircaCP ----------------
  CPdf  <- CircaCP::screen_wear (df = sbData,
                                 min_days = NDays,
                                 max_zero_run = ScreenEpoch)

  if (CPdf$status != "ok") {
    stop ("CircaCP failed to identify sufficient length of contiguous active
    segment. Returning NA for all epochs.")
    return (rep (NA_character_, nrow (data)))

  } else {

  SW    <- CircaCP::sleep_detection (clean_df = CPdf$clean_df,
                                     thr      = CutPoint,
                                     dist     = Dist)

  # Step 4. Restructure and Labeling -----------------
  Out <- rep (NA_character_, nrow (data))
  Out [!Temp$pNW > CutpNW] <- ifelse (SW$label.sw == 0, "S", "W")

  if (Insufficient) Out <- paste0 (Out, "_", NDays)

  return (Out)
  }
}






#' @title Oakley's Algorithm for Sleep-Wake Scoring
#'
#' @importFrom stats filter
#'
#'
#' @param y  Numeric vector of activity counts, one value per epoch.
#'
#' @param Epc Integer. Epoch length in seconds: 15, 30, 60, or 120.
#'
#' @param CutPoint Numeric scalar OR the string "automatic".
#'   Numeric: epochs with weighted score < threshold → sleep.
#'   "automatic": Actiware Auto Wake Threshold computed from data.
#'   Default is 40.
#'
#' @param Scaler  Numeric. Multiplier for automatic threshold only.
#'   Default is 0.88888 (per Actiware manual).
#'
#' @param Pad Character scalar. Padding method for edge epochs where the full
#' window overhangs. Available methods include:
#' \itemize{
#'  \item NA (default, no padding): edge epochs return NA
#'  \item Reflect: pad by reflecting the data at the edges (e.g., y[2], y[3],
#'  ... for left edge)
#'  \item Zero: pad with zeros at the edges
#' }
#'
#' @return Integer vector (same length as y):
#'   1 = sleep, 0 = wake, NA = edge epoch (insufficient window).
#'
#' @references
#' Actiwatch Communication and Sleep Analysis Software instruction manual
#' (Respironics, Inc.)
#'
#' https://github.com/ghammad/pyActigraphy
#'
#' @noRd

Oakley <- function (y, Epc, CutPoint = 40L, Scaler = 0.88888,
                    Pad = NA) {
  # Step 0: Check Point and Parameter Extraction ---------------
  ## Input validation ----
  ### Epc
  Epc <- round (Epc)
  if (!Epc %in% c (15L, 30L, 60L, 120L)) {
    stop (
      "Oakley's algorithm is not defined for epoch length ", Epc,
      " seconds. Accepted values: 15, 30, 60, 120."
    )
  }

  ### CutPoint.Oakley
  if (inherits (CutPoint, "character")) {
    if (tolower (CutPoint) != "automatic") {
      stop ("`CutPoint.Oakley` must be numeric or the string 'automatic'.")
    }
    CutPoint <- .oakley_auto_threshold (y, Epc, Scaler)
    message ("Oakley: automatic wake CutPoint.Oakley = ",
             round(CutPoint, 4))
  }
  if (inherits (CutPoint, "numeric") && length (CutPoint) > 1) {
    stop ("`CutPoint.Oakley` must be a single numeric value or 'automatic'.")
  }

  Len <- length (y)

  ## Weighting coefficients (Actiwatch manual / pyActigraphy)
  Window <- switch (
    as.character (Epc),
    "15" = c (
      0.04, 0.04, 0.04, 0.04,   # W_{-8} .. W_{-5}  = 1/25
      0.20, 0.20, 0.20, 0.20,   # W_{-4} .. W_{-1}  = 1/5
      4.00,                       # W_{ 0}
      0.20, 0.20, 0.20, 0.20,   # W_{+1} .. W_{+4}  = 1/5
      0.04, 0.04, 0.04, 0.04    # W_{+5} .. W_{+8}  = 1/25
    ),
    "30" = c (
      0.04, 0.04,               # W_{-4}, W_{-3}    = 1/25
      0.20, 0.20,               # W_{-2}, W_{-1}    = 1/5
      2.00,                     # W_{ 0}
      0.20, 0.20,               # W_{+1}, W_{+2}    = 1/5
      0.04, 0.04                # W_{+3}, W_{+4}    = 1/25
    ),
    "60" = c(
      0.04,                     # W_{-2}            = 1/25
      0.20,                     # W_{-1}            = 1/5
      1.00,                     # W_{ 0}
      0.20,                     # W_{+1}            = 1/5
      0.04                      # W_{+2}            = 1/25
    ),
    "120" = c (
      0.12,                     # W_{-1}            = 1/8
      0.50,                     # W_{ 0}            = 1/2
      0.12                      # W_{+1}            = 1/8
    )
  )


  # Step 1. Edge Padding -----
  hw <- (length (Window) - 1L) %/% 2L
  y_pad <- Padding (x = y, Window = hw, Pad = Pad)

  # Step 2. Centered weighted moving average ------------------
  SWscore_pad <- as.numeric (stats::filter (y_pad, filter = Window,
                                            sides = 2L))
  SWscore     <- SWscore_pad [(hw + 1) : (hw + Len)]
  # trim to original length


  # Step 3. Classification --------------------
  Out <- ifelse (SWscore < CutPoint, "S", "W")

  return (Out)
}





#' @title Internal Helper Actiware Automatic Wake CutPoint.Oakley
#' @noRd
.oakley_auto_threshold <- function (y, Epc, Scaler = 0.88888) {
  # Step 0: Check Point
  if (pmax (Scaler, 0) == 0)
    stop ("`Scaler` must be a positive numeric value.")


  # Step 1: Sum all activity counts
  CSum <- sum (y, na.rm = TRUE)

  # Step 2: MOBILE CutPoint.Oakley = Epc / 15
  #   An epoch is MOBILE if y >= this value.
  CutMobile <- as.integer (Epc / 15L)

  # Step 3: Count MOBILE epochs
  NMobile <- sum (y >= CutMobile, na.rm = TRUE)

  if (NMobile == 0L) {
    warning ("No MOBILE epochs found; returning default CutPoint.Oakley = 40.")
    return (40)
  }

  # Step 4: MOBILE TIME in minutes
  TMobile <- NMobile * (Epc / 60L)

  # Step 5: Auto Wake CutPoint.Oakley
  Out <- (CSum / TMobile) * Scaler

  return (Out)
}
