#  File ActiGlobe/R/ggCosinorM.R
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
#' @title Plot `CosinorM` and `CosinorM.KDE` Fit with `ggplot2`
#'
#' @description
#' Create visualization of a CosinorM or CosinorM.KDE fit using
#' ggplot2. The plot shows the parametric cosinor fit over a fine time
#' grid, optional pointwise confidence bands, observed data points,
#' MESOR line, acrophase verticals, amplitude annotation segments, and
#' labelled parameter values when requested.
#'
#' @import ggplot2
#' @importFrom stats coef df.residual model.frame model.response qt vcov
#' @importFrom ggrepel geom_label_repel
#' @importFrom viridis scale_color_viridis scale_fill_viridis
#' @importFrom scales alpha
#' @importFrom utils globalVariables
#'
#' @param object A fitted model of class \code{\link{CosinorM}} or
#'   \code{\link{CosinorM.KDE}}.
#' @param labels Logical; Default: TRUE, which places repelled labels on
#'   the plot with MESOR, amplitude(s), and acrophase(s).
#' @param ci Logical; Default: TRUE, which computes and draws pointwise
#'   parametric confidence bands for the fitted cosinor curve using
#'   the model covariance.
#' @param ci_level Numeric scaler. The range of the confidence interval,
#' 	 expressed in numeric value between 0 and 1. Default: 0.95
#' @param n Integer scaler. Number of points on the fine prediction grid used
#'   to draw the fitted cosinor and confidence ribbon. Default: 400
#' @param point_size Numeric scaler. Plotting size for observed points.
#'   Default: 0.5
#' @param title_extra Optional character string appended to the plot
#'   title for extra context.
#' @param legend.position Position of the legend on the plot; default
#'   is "right". Other options include "top", "bottom", "left",
#'   or a numeric vector of length two specifying x and y coordinates.
#' @param ... Additional arguments (currently ignored) kept for future
#'   update.
#'
#' @returns
#' A `ggplot` object representing the cosinor model fit visualization.
#'
#' @examples
#' \dontrun{
#' # Import data
#' data (FlyEast)
#'
#' BdfList <-
#'   BriefSum (
#'     df = FlyEast,
#'     SR = 1 / 60,
#'     Start = "2017-10-24 13:45:00"
#'   )
#'
#' # Let's extract actigraphy data from a single day
#' df <- BdfList$df
#' df <- subset (df, df$Date == "2017-10-28")
#'
#' fit <- CosinorM (
#'   time = df$Time,
#'   activity = df$Activity,
#'   tau = 24,
#'   method = "OLS"
#' )
#'
#'
#' p <- ggCosinorM (
#'   object = fit,
#'   labels = TRUE,
#'   ci = TRUE,
#'   ci_level = 0.95,
#'   title_extra = "2017-10-24"
#' )
#' print (p)
#' }
#'
#' @seealso
#' \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggrepel]{geom_label_repel}},
#' \code{\link[stats]{predict}}
#' @keywords plot ggplot graph cosinor
#' @export

ggCosinorM <- function (object, labels = TRUE, ci = TRUE, ci_level = 0.95,
            n = 400, point_size = 0.5, title_extra = NULL,
            legend.position = "right", ...) {
  # Step 0 Validate inputs ------------------------
  CheckInput (object, ci_level, n)

  # Step 1 Extract Essential Parameters from Model Object ---------------
  Day <- 24
  Tau <- if (!is.null (object$tau)) object$tau else 24
  nTau <- length (Tau)

  ## Extract observed data
  ObsData <- GetObsData (object)

  ## Determine parameter source
  CoefCos <- object$coef.cosinor
  PostHoc <- object$post.hoc
  UseParametricSingle <- inherits (object, "CosinorM") && nTau == 1
  UsePostHoc <- !UseParametricSingle

  # Step 2 Extract Cosinor Parameters ------------------------
  MESOR <- GetParam ("MESOR", UsePostHoc, CoefCos, PostHoc, Tau)
  Amplitude <- GetParam ("Amplitude", UsePostHoc, CoefCos, PostHoc, Tau)
  AcrophaseTime <- GetParam ("Acrophase", UsePostHoc, CoefCos, PostHoc, Tau)
  Bathy <- GetParam ("Bathyphase", UsePostHoc, PostHoc, NULL, Tau, Day,
             AcrophaseTime)
  PeakTroughVals <- GetParam ("PeakTrough", UsePostHoc, PostHoc, NULL, NULL,
                NULL, NULL, MESOR, Amplitude)
  PeakValue <- PeakTroughVals$peak
  TroughValue <- PeakTroughVals$trough

  # Step 3 Compute Fitted Curve ------------------------
  FitResult <- GetFit (object, ObsData, Tau, n)
  NewT <- FitResult$newt
  FitPred <- FitResult$fit_pred
  SeFit <- FitResult$se_fit
  UsesKdf <- FitResult$uses_kdf

  # Step 4 Compute Confidence Bands ------------------------
  CiResult <- GetCI (ci, UsesKdf, FitPred, SeFit, ci_level, object)
  YMin <- CiResult$ym
  YMax <- CiResult$yM

  # Step 5 Fallback Markers from Fitted Curve ------------------------
  FallbackMarkers <- FallbackFit (Bathy, PeakValue, TroughValue,
                  AcrophaseTime, NewT, FitPred)
  Bathy <- FallbackMarkers$bathy
  PeakValue <- FallbackMarkers$peak_value
  TroughValue <- FallbackMarkers$trough_value
  AcrophaseTime <- FallbackMarkers$acrophase_time

  # Step 6 Build Plot ------------------------
  GGplot <- ggplot2::ggplot ()
  GGplot <- PlotFit (GGplot, NewT, FitPred, ci, YMin, YMax)
  GGplot <- PlotMESOR (GGplot, MESOR, ObsData, point_size, AcrophaseTime,
             PeakValue, Bathy, TroughValue)
  GGplot <- PlotAcro (GGplot, AcrophaseTime, ObsData)
  GGplot <- PlotInactive (GGplot, ObsData)
  GGplot <- PlotAmp (GGplot, Amplitude, AcrophaseTime, MESOR)
  GGplot <- PlotLabels (GGplot, labels, UsePostHoc, Tau, MESOR, Amplitude,
              AcrophaseTime, ObsData)
  GGplot <- PlotTheme (GGplot, legend.position, Tau, object, title_extra, Day)

  return (GGplot)
}


# Pre-defined helper functions -------------------------------
#' @title Check Input
#' @description
#' Validates that the object is of correct class and that ci_level and
#' n parameters are within acceptable ranges.
#' @param object A fitted model object to validate.
#' @param ci_level Confidence level to validate.
#' @param n Number of grid points to validate.
#' @noRd
CheckInput <- function (object, ci_level, n) {
  if (!inherits (object, c ("CosinorM", "CosinorM.KDE")))
    stop ("ggCosinorM: object must be a CosinorM or ",
        "CosinorM.KDE fit.", call. = FALSE)

  if (!is.numeric (ci_level) || ci_level <= 0 || ci_level >= 1)
    stop ("ci_level must be a numeric value between 0 and 1")

  if (!is.numeric (n) || n <= 0 || n != round (n))
    stop ("N must be a positive integer")
}

#' @title Get Observed Data
#' @description
#' Extracts time and activity observations from a fitted CosinorM or
#' CosinorM.KDE model object.
#' @param object A fitted model object.
#' @returns A data.frame with columns t_obs and y_obs.
#' @noRd
GetObsData <- function (object) {
  if (!is.null (object$model) && !is.null (object$model$time)) {
    TimeObs <- object$model$time
    ActivityObs <- if (!is.null (object$model$activity)) {
      object$model$activity
    } else {
      stats::model.response (stats::model.frame (object))
    }
  } else {
    stop ("Object missing model$time/activity for ",
        "observed data.", call. = FALSE)
  }
  data.frame (t_obs = TimeObs, y_obs = ActivityObs)
}

#' @title Get Cosinor Parameter
#' @description
#' Unified function to extract cosinor parameters with switch statement.
#' @param param Parameter name: "MESOR", "Amplitude", "Acrophase",
#'   "Bathyphase", or "PeakTrough"
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates
#' @param coef_cos Cosinor coefficients vector (for MESOR, Amplitude, Acrophase)
#' @param post Post-hoc estimates vector
#' @param tau Period(s) of the cosinor model
#' @param day Length of day (for Bathyphase)
#' @param acrophase_time Acrophase time (for Bathyphase)
#' @param mesor MESOR value (for PeakTrough)
#' @param amplitude Amplitude value (for PeakTrough)
#' @returns Numeric value or list depending on param type.
#' @noRd
GetParam <- function (param, use_posthoc, coef_cos = NULL, post = NULL,
            tau = NULL, day = NULL, acrophase_time = NULL,
            mesor = NULL, amplitude = NULL) {
  switch (param,
      "MESOR" = {
        if (use_posthoc) {
          if (is.null (post) || is.null (post ["MESOR.ph"]))
            stop ("Post-hoc MESOR.ph is required.", call. = FALSE)
          as.numeric (post ["MESOR.ph"])
        } else {
          if (is.null (coef_cos) || is.null (coef_cos ["MESOR"]))
            stop ("Parametric MESOR is required.", call. = FALSE)
          as.numeric (coef_cos ["MESOR"])
        }
      },
      "Amplitude" = {
        if (use_posthoc) {
          if (is.null (post) || is.null (post ["Amplitude.ph"]))
            stop ("Post-hoc Amplitude.ph is required.",
                call. = FALSE)
          as.numeric (post ["Amplitude.ph"])
        } else {
          AmpName <- paste0 ("Amplitude.", tau)
          if (!all (AmpName %in% names (coef_cos)))
            stop ("Parametric amplitude not found.", call. = FALSE)
          as.numeric (coef_cos [AmpName])
        }
      },
      "Acrophase" = {
        if (use_posthoc) {
          if (is.null (post) || is.null (post ["Acrophase.ph.time"]))
            stop ("Post-hoc Acrophase.ph.time is required.",
                call. = FALSE)
          as.numeric (post ["Acrophase.ph.time"])
        } else {
          PhiName <- paste0 ("Acrophase.", tau)
          if (!all (PhiName %in% names (coef_cos)))
            stop ("Parametric acrophase not found.", call. = FALSE)
          AcrophaseRad <- as.numeric (coef_cos [PhiName])
          ((AcrophaseRad * tau / (2 * pi)) %% tau)
        }
      },
      "Bathyphase" = {
        if (use_posthoc) {
          if (is.null (post) || is.null (post ["Bathyphase.ph.time"])) {
            NA_real_
          } else {
            rep (as.numeric (post ["Bathyphase.ph.time"]))
          }
        } else {
          (acrophase_time - tau / 2) %% day
        }
      },
      "PeakTrough" = {
        if (use_posthoc) {
          PeakValue <- ifelse (!is.null (post ["Peak.ph"]),
                     as.numeric (post ["Peak.ph"]),
                     NA_real_)
          TroughValue <- ifelse (!is.null (post ["Trough.ph"]),
                       as.numeric (post ["Trough.ph"]),
                       NA_real_)
        } else {
          PeakValue <- mesor + amplitude
          TroughValue <- mesor - amplitude
        }
        list (peak = PeakValue, trough = TroughValue)
      },
      stop ("Invalid param: ", param,
          ". Must be one of: MESOR, Amplitude, ",
          "Acrophase, Bathyphase, PeakTrough", call. = FALSE)
  )
}

#' @title Get Parametric Fit
#' @description
#' Computes the fitted values and standard errors for a parametric
#' cosinor model over a fine time grid.
#' @param object A fitted CosinorM object.
#' @param aug Data frame with observed data.
#' @param tau Period(s) of the cosinor model.
#' @param n Number of grid points for prediction.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf.
#' @noRd
GetParamFit <- function (object, aug, tau, n) {
  nTau <- length (tau)
  TimeMin <- min (aug$t_obs, na.rm = TRUE)
  TimeMax <- max (aug$t_obs, na.rm = TRUE)
  NewT <- seq (TimeMin, TimeMax, length.out = n)

  NewData <- data.frame (t_obs = NewT)
  for (i in seq_len (nTau)) {
    NewData [[paste0 ("C", i)]] <- cos (2 * pi * NewT / tau [i])
    NewData [[paste0 ("S", i)]] <- sin (2 * pi * NewT / tau [i])
  }
  MatrixX <- as.matrix (cbind (
    "(Intercept)" = 1,
    NewData [, grepl ("^C|^S", names (NewData)), drop = FALSE]
  ))

  CoefLm <- tryCatch (stats::coef (object), error = function (e) numeric ())
  CoefVec <- numeric (ncol (MatrixX))
  names (CoefVec) <- colnames (MatrixX)
  CommonNames <- intersect (names (CoefLm), names (CoefVec))
  CoefVec [CommonNames] <- CoefLm [CommonNames]
  FitPred <- as.numeric (MatrixX %*% CoefVec)

  VcovMat <- tryCatch (
    {
      if (!is.null (object$vcov)) object$vcov else stats::vcov (object)
    },
    error = function (e) NULL
  )
  if (!is.null (VcovMat)) {
    if (!all (colnames (VcovMat) %in% colnames (MatrixX))) {
      FullVcov <- matrix (0,
                nrow = ncol (MatrixX), ncol = ncol (MatrixX),
                dimnames = list (colnames (MatrixX),
                         colnames (MatrixX))
      )
      CommonV <- intersect (rownames (VcovMat), rownames (FullVcov))
      FullVcov [CommonV, CommonV] <- VcovMat [CommonV, CommonV]
      VcovMat <- FullVcov
    } else {
      VcovMat <- VcovMat [colnames (MatrixX), colnames (MatrixX)]
    }
    SeFit <- sqrt (rowSums ((MatrixX %*% VcovMat) * MatrixX))
  } else {
    SeFit <- rep (NA_real_, length (FitPred))
  }

  list (newt = NewT, fit_pred = FitPred, se_fit = SeFit, uses_kdf = FALSE)
}

#' @title Get KDE Fit
#' @description
#' Extracts fitted values and standard errors from a CosinorM.KDE
#' model object.
#' @param object A fitted CosinorM.KDE object.
#' @param t_obs Observed time points.
#' @param tau Period(s) of the cosinor model.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf, or NULL.
#' @noRd
GetKDEFit <- function (object, t_obs, tau) {
  if (!is.null (object$kdf)) {
    Kdf <- object$kdf
    NewT <- t_obs
    FitPred <- Kdf$fitted.values
    SeFit <- if (!is.null (Kdf$fitted.se))
      Kdf$fitted.se else rep (NA_real_, length (FitPred))
    list (newt = NewT, fit_pred = FitPred, se_fit = SeFit, uses_kdf = TRUE)
   } else {
    NULL
  }
}

#' @title Get Fit
#' @description
#' Determines whether to use KDE or parametric fit and computes the
#' fitted curve accordingly.
#' @param object A fitted model object.
#' @param aug Data frame with observed data.
#' @param tau Period(s) of the cosinor model.
#' @param n Number of grid points for prediction.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf.
#' @noRd
GetFit <- function (object, aug, tau, n) {
  if (inherits (object, "CosinorM.KDE")) {
    KDEResult <- GetKDEFit (object, aug$t_obs, tau)
    if (!is.null (KDEResult)) return (KDEResult)
  }
  GetParamFit (object, aug, tau, n)
}

#' @title Get CI
#' @description
#' Computes pointwise confidence bands for the fitted cosinor curve
#' using either z-critical values (KDE) or t-critical values
#' (parametric).
#' @param ci Logical; if TRUE, compute confidence bands.
#' @param uses_kdf Logical; if TRUE, use z-critical values.
#' @param fit_pred Fitted values.
#' @param se_fit Standard errors of fitted values.
#' @param ci_level Confidence level.
#' @param object A fitted model object.
#' @returns A list with ym (lower bound) and yM (upper bound).
#' @noRd
GetCI <- function (ci, uses_kdf, fit_pred, se_fit,
           ci_level, object) {
  if (!ci) {
    return (list (ym = rep (NA_real_, length (fit_pred)),
            yM = rep (NA_real_, length (fit_pred))))
  }

  if (uses_kdf) {
    ZCrit <- 1.96
    YMin <- fit_pred - ZCrit * se_fit
    YMax <- fit_pred + ZCrit * se_fit
  } else {
    Alpha <- 1 - ci_level
    TCrit <- stats::qt (1 - Alpha / 2, df = stats::df.residual (object))
    YMin <- fit_pred - TCrit * se_fit
    YMax <- fit_pred + TCrit * se_fit
  }
  list (ym = YMin, yM = YMax)
}

#' @title Fallback Fit
#' @description
#' Fills in missing peak, trough, and acrophase markers using the
#' fitted curve's maximum and minimum values.
#' @param bathy Bathyphase time value.
#' @param peak_value Peak activity value.
#' @param trough_value Trough activity value.
#' @param acrophase_time Acrophase time value.
#' @param newt Time points of fitted curve.
#' @param fit_pred Fitted values.
#' @returns A list with updated bathy, peak_value, trough_value, and
#'   acrophase_time.
#' @noRd
FallbackFit <- function (bathy, peak_value, trough_value,
             acrophase_time, newt, fit_pred) {
  if (any (is.na (bathy) || is.na (peak_value) || is.na (trough_value))) {
    MaxIdx <- which.max (fit_pred)
    MinIdx <- which.min (fit_pred)
    bathy <- ifelse (is.na (bathy), newt [MinIdx], bathy)
    peak_value <- ifelse (is.na (peak_value), fit_pred [MaxIdx], peak_value)
    trough_value <- ifelse (is.na (trough_value), fit_pred [MinIdx],
                trough_value)
    if ((length (acrophase_time) == 0 || is.na (acrophase_time [1]))) {
      acrophase_time <- newt [MaxIdx]
    }
  }
  list (bathy = bathy, peak_value = peak_value, trough_value = trough_value,
      acrophase_time = acrophase_time)
}

#' @title Plot Fit
#' @description
#' Adds the fitted cosinor curve and optional confidence ribbon to
#' a ggplot object.
#' @param g A ggplot object.
#' @param newt Time points for fitted curve.
#' @param fit_pred Fitted values.
#' @param ci Logical; if TRUE, add confidence ribbon.
#' @param ym Lower confidence bound.
#' @param yM Upper confidence bound.
#' @returns Updated ggplot object.
#' @noRd
PlotFit <- function(g, newt, fit_pred, ci, ym, yM) {
  g <- g + ggplot2::geom_line (
    ggplot2::aes (x = newt, y = fit_pred, colour = "Model Fit"),
    linewidth = 0.9,
    inherit.aes = FALSE
  )

  if (ci && any (is.finite (ym) & is.finite (yM))) {
    g <- g + ggplot2::geom_ribbon (
      ggplot2::aes (x = newt, ymin = ym, ymax = yM, fill = "Fit CI"),
      colour = NA,
      alpha = 0.18,
      inherit.aes = FALSE
    )
  }
  g
}

#' @title Plot MESOR
#' @description
#' Adds MESOR horizontal line, observed data points, peak and trough
#' markers to a ggplot object.
#' @param g A ggplot object.
#' @param mesor MESOR value.
#' @param aug Data frame with observed data.
#' @param point_size Size for observed data points.
#' @param acrophase_time Acrophase time value.
#' @param peak_value Peak activity value.
#' @param bathy Bathyphase time value.
#' @param trough_value Trough activity value.
#' @returns Updated ggplot object.
#' @noRd
PlotMESOR <- function(g, mesor, aug, point_size, acrophase_time,
            peak_value, bathy, trough_value) {
  g <- g +
    ggplot2::geom_hline (
      ggplot2::aes (yintercept = mesor, colour = "MESOR"),
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point (
      data = aug,
      ggplot2::aes (x = t_obs, y = y_obs, colour = "Observed"),
      alpha = 0.5,
      size = point_size,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point (
      ggplot2::aes (x = acrophase_time, y = peak_value, colour = "Peak"),
      shape = 16,
      size = 3,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point (
      ggplot2::aes (x = bathy, y = trough_value, colour = "Trough"),
      shape = 18,
      size = 3,
      inherit.aes = FALSE
    )
  g
}

#' @title Plot Acrophase
#' @description
#' Adds vertical line(s) at the acrophase time(s) to a ggplot object.
#' @param g A ggplot object.
#' @param acrophase_time Acrophase time value(s).
#' @param aug Data frame with observed data.
#' @returns Updated ggplot object.
#' @noRd
PlotAcro <- function(g, acrophase_time, aug) {
  y_sf <- floor (max (aug$y_obs, na.rm = TRUE) * 0.8)
  Ay <- seq_len (y_sf)
  Ax <- rep (acrophase_time, each = length (Ay))

  g + ggplot2::geom_line (
    ggplot2::aes (x = Ax, y = Ay, colour = "Acrophase"),
    linewidth = 0.9,
    inherit.aes = FALSE
  )
}

#' @title Plot Inactive
#' @description
#' Identifies and adds shaded rectangles for detected inactive periods
#' to a ggplot object.
#' @param g A ggplot object.
#' @param aug Data frame with observed data.
#' @returns Updated ggplot object.
#' @noRd
PlotInactive <- function(g, aug) {
  y_sf <- floor (max (aug$y_obs, na.rm = TRUE) * 0.8)
  inatv <- Prob.Inact (
    y = aug$y_obs,
    time = aug$t_obs,
    k = 12,
    threshold = 3,
    logical = FALSE
  )

  if (!nrow (inatv) == 0) {
    Ymin <- rep (0, nrow (inatv))
    Ymax <- rep (y_sf, nrow (inatv))
    start <- inatv$start
    end <- inatv$end

    g <- g + ggplot2::geom_rect (
      ggplot2::aes (
        xmin = start, xmax = end,
        ymin = Ymin, ymax = Ymax,
        fill = "Inactive Period"
      ),
      inherit.aes = FALSE,
      alpha = 0.1
    )
  }
  g
}

#' @title Plot Amplitude
#' @description
#' Adds dashed line segments to visualize the amplitude from MESOR to
#' peak value at acrophase time(s).
#' @param g A ggplot object.
#' @param amplitude Amplitude value(s).
#' @param acrophase_time Acrophase time value(s).
#' @param mesor MESOR value.
#' @returns Updated ggplot object.
#' @noRd
PlotAmp <- function(g, amplitude, acrophase_time, mesor) {
  if (length (amplitude) >= 1 && all (is.finite (amplitude))) {
    for (i in seq_len (length (amplitude))) {
      g <- g + ggplot2::geom_segment (
        mapping = ggplot2::aes (
          x = acrophase_time [i], xend = acrophase_time [i],
          y = mesor, yend = mesor + amplitude [i]
        ),
        linetype = "twodash", lineend = "butt", linejoin = "mitre",
        inherit.aes = FALSE,
        colour = "grey50"
      ) +
        ggplot2::geom_segment (
          mapping = ggplot2::aes (
            x = 0, xend = acrophase_time [i],
            y = mesor + amplitude [i], yend = mesor + amplitude [i]
          ),
          linetype = "twodash", lineend = "butt", linejoin = "mitre",
          inherit.aes = FALSE,
          colour = "grey50"
        )
    }
  }
  g
}

#' @title Plot Labels
#' @description
#' Adds repelled text labels showing MESOR, amplitude, and acrophase
#' values to a ggplot object.
#' @param g A ggplot object.
#' @param labels Logical; if TRUE, add labels.
#' @param use_posthoc Logical; if TRUE, label as post-hoc.
#' @param tau Period(s) of the cosinor model.
#' @param mesor MESOR value.
#' @param amplitude Amplitude value(s).
#' @param acrophase_time Acrophase time value(s).
#' @param aug Data frame with observed data.
#' @returns Updated ggplot object.
#' @noRd
PlotLabels <- function(g, labels, use_posthoc, tau, mesor, amplitude,
             acrophase_time, aug) {
  if (!labels) return(g)

  Tl <- if (use_posthoc) "post.hoc" else tau
  Cof.Names <- c ("MESOR", paste0 ("Amplitude.", Tl),
          paste0 ("Acrophase.", Tl))
  Value <- c (mesor, amplitude [1], acrophase_time [1])
  xAxis <- c (min (aug$t_obs, na.rm = TRUE), acrophase_time [1],
        acrophase_time [1])
  yAxis <- c (mesor, (mesor + amplitude [1]) * 4 / 5,
        mesor + amplitude [1])

  g + ggrepel::geom_label_repel (
    ggplot2::aes (x = xAxis, y = yAxis,
            label = paste0 (Cof.Names, " = ", round (Value, 2))),
    label.size = NA, label.r = 0.25, label.padding = 0.25,
    force = 10, segment.color = "transparent", fontface = "bold",
    inherit.aes = FALSE
  )
}

#' @title Plot Theme
#' @description
#' Adds color/fill scales, theme settings, axis labels, and plot title
#' to a ggplot object.
#' @param g A ggplot object.
#' @param legend.position Position for the legend.
#' @param tau Period(s) of the cosinor model.
#' @param object A fitted model object.
#' @param title_extra Optional extra text for title.
#' @param day Length of day in hours (typically 24).
#' @returns Updated ggplot object.
#' @noRd
PlotTheme <- function(g, legend.position, tau, object,
            title_extra, day = 24) {
  legend_keys <- c (
    "Observed", "Peak", "Trough", "Model Fit",
    "MESOR", "Acrophase"
  )

  g <- g +
    ggplot2::scale_colour_manual (
      name = "",
      values = c (
        "Model Fit" = "blue",
        "MESOR"   = "red",
        "Observed"  = "black",
        "Acrophase" = "purple",
        "Peak"    = "black",
        "Trough"  = "black"
      ),
      breaks = legend_keys
    ) +
    ggplot2::scale_fill_manual (
      name = "",
      values = c (
        "Fit CI" = scales::alpha ("cyan", 0.18),
        "Inactive Period" = scales::alpha ("orange", 0.1)
      ),
      breaks = c ("Fit CI", "Inactive Period")
    )

  essential_tau <- if (length (tau) == 1) as.character (tau) else
    paste (tau, collapse = ",")
  method_lbl <- if (!is.null (object$method)) object$method else
    if (inherits (object, "CosinorM.KDE")) "KDE" else "Parametric"
  essential <- paste0 ("(", essential_tau, "Hour) - ", method_lbl)
  parts <- c (class (object) [1])
  if (!is.null (title_extra) && nzchar (title_extra))
    parts <- c (parts, title_extra)
  parts <- c (parts, essential)
  plot_title <- paste (parts, collapse = " ")

  g + ggplot2::theme_minimal () +
    ggplot2::theme (
      legend.position = legend.position,
      panel.grid.major = ggplot2::element_blank (),
      panel.grid.minor = ggplot2::element_blank (),
      plot.title = ggplot2::element_text (
        hjust = 0.5,
        face = "bold",
        size = 16,
        margin = ggplot2::margin (b = 15)),
      legend.margin =  ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::labs (x = "Time", y = "Activity", title = plot_title) +
    ggplot2::scale_x_continuous (breaks = seq (0, day, by = 6),
                   expand = c (0, 0)) +
    ggplot2::coord_cartesian (xlim = c (0, day))
}

# Suppress CMD check notes about global variables
utils::globalVariables(c("t_obs", "y_obs"))
