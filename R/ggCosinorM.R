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
#' @import stats ggplot2 ggrepel viridis
#' @importFrom scales alpha
#'
#' @param object A fitted model of class \code{\link{CosinorM}} or
#'   \code{\link{CosinorM.KDE}}.
#' @param labels Logical; Default `TRUE` places repelled labels on
#'   the plot with MESOR, amplitude(s), and acrophase(s).
#' @param ci Logical; Default `TRUE` computes and draws pointwise
#'   parametric confidence bands for the fitted cosinor curve using
#'   the model covariance.
#' @param ci_level Confidence level for the pointwise bands, expressed
#'   in numeric value between 0 and 1 (default: `0.95`).
#' @param n Integer; number of points on the fine prediction grid used
#'   to draw the fitted cosinor and confidence ribbon (default: `400`).
#' @param point_size Numeric; plotting size for observed points
#'   (default: `0.5`).
#' @param title_extra Optional character string appended to the plot
#'   title for extra context.
#' @param legend.position Position of the legend on the plot; default
#'   is `"right"`. Other options include `"top"`, `"bottom"`, `"left"`,
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
#'     BriefSum (
#'         df = FlyEast,
#'         SR = 1 / 60,
#'         Start = "2017-10-24 13:45:00"
#'     )
#'
#' # Let's extract actigraphy data from a single day
#' df <- BdfList$df
#' df <- subset (df, df$Date == "2017-10-28")
#'
#' fit <- CosinorM (
#'     time = df$Time,
#'     activity = df$Activity,
#'     tau = 24,
#'     method = "OLS"
#' )
#'
#'
#' p <- ggCosinorM (
#'     object = fit,
#'     labels = TRUE,
#'     ci = TRUE,
#'     ci_level = 0.95,
#'     title_extra = "2017-10-24"
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
    # Validate inputs
    .validate_ggcosinorm_inputs (object, ci_level, n)

    # Extract basic parameters
    day <- 24
    tau <- if (!is.null (object$tau)) object$tau else 24
    nT <- length (tau)

    # Extract observed data
    aug <- .extract_observed_data (object)

    # Determine parameter source
    coef_cos <- object$coef.cosinor
    post <- object$post.hoc
    use_parametric_single <- inherits (object, "CosinorM") && nT == 1
    use_posthoc <- !use_parametric_single

    # Extract cosinor parameters
    mesor <- .get_mesor (object, use_posthoc, coef_cos, post)
    amplitude <- .get_amplitude (object, use_posthoc, coef_cos, post, tau)
    acrophase_time <- .get_acrophase_time (
        object, use_posthoc, coef_cos,
        post, tau
    )
    bathy <- .get_bathyphase (
        object, use_posthoc, post, acrophase_time,
        tau, day
    )
    pv <- .get_peak_trough_values (use_posthoc, post, mesor, amplitude)
    peak_value <- pv$peak
    trough_value <- pv$trough

    # Compute fitted curve
    fit_result <- .compute_fitted_curve (object, aug, tau, n)
    newt <- fit_result$newt
    fit_pred <- fit_result$fit_pred
    se_fit <- fit_result$se_fit
    uses_kdf <- fit_result$uses_kdf

    # Compute confidence bands
    ci_result <- .compute_confidence_bands (
        ci, uses_kdf, fit_pred, se_fit,
        ci_level, object
    )
    ym <- ci_result$ym
    yM <- ci_result$yM

    # Fallback markers from fitted curve if needed
    fb <- .fallback_markers_from_fit (
        bathy, peak_value, trough_value,
        acrophase_time, newt, fit_pred
    )
    bathy <- fb$bathy
    peak_value <- fb$peak_value
    trough_value <- fb$trough_value
    acrophase_time <- fb$acrophase_time

    # Build plot
    g <- ggplot2::ggplot ()
    g <- .add_model_fit_and_ci (g, newt, fit_pred, ci, ym, yM)
    g <- .add_mesor_and_points (
        g, mesor, aug, point_size, acrophase_time,
        peak_value, bathy, trough_value
    )
    g <- .add_acrophase_verticals (g, acrophase_time, aug)
    g <- .add_inactive_periods (g, aug)
    g <- .add_amplitude_segments (g, amplitude, acrophase_time, mesor)
    g <- .add_labels (
        g, labels, use_posthoc, tau, mesor, amplitude,
        acrophase_time, aug
    )
    g <- .add_scales_and_theme (
        g, legend.position, tau, object, title_extra,
        day
    )

    return (g)
}


#' @title Validate Inputs for ggCosinorM
#' @description
#' Validates that the object is of correct class and that ci_level and
#' n parameters are within acceptable ranges.
#' @param object A fitted model object to validate.
#' @param ci_level Confidence level to validate.
#' @param n Number of grid points to validate.
#' @noRd
.validate_ggcosinorm_inputs <- function (object, ci_level, n) {
    if (!inherits (object, c ("CosinorM", "CosinorM.KDE"))) {
        stop ("ggCosinorM: object must be a CosinorM or ",
            "CosinorM.KDE fit.",
            call. = FALSE
        )
    }

    if (!is.numeric (ci_level) | ci_level <= 0 | ci_level >= 1) {
        stop ("ci_level must be a numeric value between 0 and 1")
    }

    if (!is.numeric (n) | n <= 0 | n != round (n)) {
        stop ("N must be a positive integer")
    }
}

#' @title Extract Observed Data from Cosinor Model
#' @description
#' Extracts time and activity observations from a fitted CosinorM or
#' CosinorM.KDE model object.
#' @param object A fitted model object.
#' @returns A data.frame with columns t_obs and y_obs.
#' @noRd
.extract_observed_data <- function (object) {
    if (!is.null (object$model) && !is.null (object$model$time)) {
        t_obs <- object$model$time
        y_obs <- if (!is.null (object$model$activity)) {
            object$model$activity
        } else {
            stats::model.response (stats::model.frame (object))
        }
    } else {
        stop ("Object missing model$time/activity for ",
            "observed data.",
            call. = FALSE
        )
    }
    data.frame (t_obs = t_obs, y_obs = y_obs)
}

#' @title Extract MESOR Parameter
#' @description
#' Extracts the MESOR (Midline Estimating Statistic Of Rhythm) from
#' either post-hoc or parametric cosinor coefficients.
#' @param object A fitted model object.
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates.
#' @param coef_cos Cosinor coefficients vector.
#' @param post Post-hoc estimates vector.
#' @returns Numeric value of MESOR.
#' @noRd
.get_mesor <- function (object, use_posthoc, coef_cos, post) {
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["MESOR.ph"])) {
            stop ("Post-hoc MESOR.ph is required.", call. = FALSE)
        }
        as.numeric (post ["MESOR.ph"])
    } else {
        if (is.null (coef_cos) || is.null (coef_cos ["MESOR"])) {
            stop ("Parametric MESOR is required.", call. = FALSE)
        }
        as.numeric (coef_cos ["MESOR"])
    }
}

#' @title Extract Amplitude Parameter
#' @description
#' Extracts the amplitude from either post-hoc or parametric cosinor
#' coefficients.
#' @param object A fitted model object.
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates.
#' @param coef_cos Cosinor coefficients vector.
#' @param post Post-hoc estimates vector.
#' @param tau Period(s) of the cosinor model.
#' @returns Numeric value(s) of amplitude.
#' @noRd
.get_amplitude <- function (object, use_posthoc, coef_cos, post, tau) {
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Amplitude.ph"])) {
            stop ("Post-hoc Amplitude.ph is required.", call. = FALSE)
        }
        as.numeric (post ["Amplitude.ph"])
    } else {
        amp_name <- paste0 ("Amplitude.", tau)
        if (!all (amp_name %in% names (coef_cos))) {
            stop ("Parametric amplitude not found.", call. = FALSE)
        }
        as.numeric (coef_cos [amp_name])
    }
}

#' @title Extract Acrophase Time
#' @description
#' Extracts the acrophase time (peak time in hours) from either
#' post-hoc or parametric cosinor coefficients.
#' @param object A fitted model object.
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates.
#' @param coef_cos Cosinor coefficients vector.
#' @param post Post-hoc estimates vector.
#' @param tau Period(s) of the cosinor model.
#' @returns Numeric value(s) of acrophase time in hours.
#' @noRd
.get_acrophase_time <- function (object, use_posthoc, coef_cos, post, tau) {
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Acrophase.ph.time"])) {
            stop ("Post-hoc Acrophase.ph.time is required.", call. = FALSE)
        }
        as.numeric (post ["Acrophase.ph.time"])
    } else {
        phi_name <- paste0 ("Acrophase.", tau)
        if (!all (phi_name %in% names (coef_cos))) {
            stop ("Parametric acrophase not found.", call. = FALSE)
        }
        acrophase_rad <- as.numeric (coef_cos [phi_name])
        ((acrophase_rad * tau / (2 * pi)) %% tau)
    }
}

#' @title Extract Bathyphase Time
#' @description
#' Extracts the bathyphase time (trough time in hours) from either
#' post-hoc estimates or calculates it from acrophase.
#' @param object A fitted model object.
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates.
#' @param post Post-hoc estimates vector.
#' @param acrophase_time Acrophase time value(s).
#' @param tau Period(s) of the cosinor model.
#' @param day Length of day in hours (typically 24).
#' @returns Numeric value(s) of bathyphase time in hours.
#' @noRd
.get_bathyphase <- function (object, use_posthoc, post, acrophase_time,
                             tau, day) {
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Bathyphase.ph.time"])) {
            NA_real_
        } else {
            rep (as.numeric (post ["Bathyphase.ph.time"]))
        }
    } else {
        (acrophase_time - tau / 2) %% day
    }
}

#' @title Extract Peak and Trough Values
#' @description
#' Extracts or calculates the peak and trough activity values.
#' @param use_posthoc Logical; if TRUE, use post-hoc estimates.
#' @param post Post-hoc estimates vector.
#' @param mesor MESOR value.
#' @param amplitude Amplitude value(s).
#' @returns A list with peak and trough values.
#' @noRd
.get_peak_trough_values <- function (use_posthoc, post, mesor, amplitude) {
    if (use_posthoc) {
        peak_value <- if (!is.null (post ["Peak.ph"])) {
            as.numeric (post ["Peak.ph"])
        } else {
            NA_real_
        }
        trough_value <- if (!is.null (post ["Trough.ph"])) {
            as.numeric (post ["Trough.ph"])
        } else {
            NA_real_
        }
    } else {
        peak_value <- mesor + amplitude
        trough_value <- mesor - amplitude
    }
    list (peak = peak_value, trough = trough_value)
}

#' @title Compute Parametric Cosinor Fit
#' @description
#' Computes the fitted values and standard errors for a parametric
#' cosinor model over a fine time grid.
#' @param object A fitted CosinorM object.
#' @param aug Data frame with observed data.
#' @param tau Period(s) of the cosinor model.
#' @param n Number of grid points for prediction.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf.
#' @noRd
.compute_parametric_fit <- function (object, aug, tau, n) {
    nT <- length (tau)
    t_min <- min (aug$t_obs, na.rm = TRUE)
    t_max <- max (aug$t_obs, na.rm = TRUE)
    newt <- seq (t_min, t_max, length.out = n)

    newdata <- data.frame (t_obs = newt)
    for (i in seq_len (nT)) {
        newdata [[paste0 ("C", i)]] <- cos (2 * pi * newt / tau [i])
        newdata [[paste0 ("S", i)]] <- sin (2 * pi * newt / tau [i])
    }
    X_new <- as.matrix (cbind (
        "(Intercept)" = 1,
        newdata [, grepl ("^C|^S", names (newdata)), drop = FALSE]
    ))

    coef_lm <- tryCatch (stats::coef (object), error = function (e) numeric ())
    coef_vec <- numeric (ncol (X_new))
    names (coef_vec) <- colnames (X_new)
    common <- intersect (names (coef_lm), names (coef_vec))
    coef_vec [common] <- coef_lm [common]
    fit_pred <- as.numeric (X_new %*% coef_vec)

    vcov_mat <- tryCatch (
        {
            if (!is.null (object$vcov)) object$vcov else stats::vcov (object)
        },
        error = function (e) NULL
    )
    if (!is.null (vcov_mat)) {
        if (!all (colnames (vcov_mat) %in% colnames (X_new))) {
            full_vcov <- matrix (0,
                nrow = ncol (X_new), ncol = ncol (X_new),
                dimnames = list (colnames (X_new), colnames (X_new))
            )
            common_v <- intersect (rownames (vcov_mat), rownames (full_vcov))
            full_vcov [common_v, common_v] <- vcov_mat [common_v, common_v]
            vcov_mat <- full_vcov
        } else {
            vcov_mat <- vcov_mat [colnames (X_new), colnames (X_new)]
        }
        se_fit <- sqrt (rowSums ((X_new %*% vcov_mat) * X_new))
    } else {
        se_fit <- rep (NA_real_, length (fit_pred))
    }

    list (newt = newt, fit_pred = fit_pred, se_fit = se_fit, uses_kdf = FALSE)
}

#' @title Compute KDE Cosinor Fit
#' @description
#' Extracts fitted values and standard errors from a CosinorM.KDE
#' model object.
#' @param object A fitted CosinorM.KDE object.
#' @param t_obs Observed time points.
#' @param tau Period(s) of the cosinor model.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf, or NULL.
#' @noRd
.compute_kde_fit <- function (object, t_obs, tau) {
    if (!is.null (object$kdf)) {
        kdf <- object$kdf
        newt <- t_obs
        fit_pred <- kdf$fitted.values
        se_fit <- if (!is.null (kdf$fitted.se)) {
            kdf$fitted.se
        } else {
            rep (NA_real_, length (fit_pred))
        }
        list (newt = newt, fit_pred = fit_pred, se_fit = se_fit, uses_kdf = TRUE)
    } else if (!is.null (object$grid)) {
        gtheta <- object$grid$theta
        newt <- (gtheta * tau) / (2 * pi)
        fit_pred <- object$grid$fitted.values
        se_fit <- object$grid$fitted.se
        list (newt = newt, fit_pred = fit_pred, se_fit = se_fit, uses_kdf = TRUE)
    } else {
        NULL
    }
}

#' @title Compute Fitted Curve
#' @description
#' Determines whether to use KDE or parametric fit and computes the
#' fitted curve accordingly.
#' @param object A fitted model object.
#' @param aug Data frame with observed data.
#' @param tau Period(s) of the cosinor model.
#' @param n Number of grid points for prediction.
#' @returns A list with newt, fit_pred, se_fit, and uses_kdf.
#' @noRd
.compute_fitted_curve <- function (object, aug, tau, n) {
    if (inherits (object, "CosinorM.KDE")) {
        kde_result <- .compute_kde_fit (object, aug$t_obs, tau)
        if (!is.null (kde_result)) {
            return (kde_result)
        }
    }
    .compute_parametric_fit (object, aug, tau, n)
}

#' @title Compute Confidence Bands
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
.compute_confidence_bands <- function (ci, uses_kdf, fit_pred, se_fit,
                                       ci_level, object) {
    if (!ci) {
        return (list (
            ym = rep (NA_real_, length (fit_pred)),
            yM = rep (NA_real_, length (fit_pred))
        ))
    }

    if (uses_kdf) {
        zcrit <- 1.96
        ym <- fit_pred - zcrit * se_fit
        yM <- fit_pred + zcrit * se_fit
    } else {
        alpha <- 1 - ci_level
        tcrit <- stats::qt (1 - alpha / 2, df = stats::df.residual (object))
        ym <- fit_pred - tcrit * se_fit
        yM <- fit_pred + tcrit * se_fit
    }
    list (ym = ym, yM = yM)
}

#' @title Fallback Markers from Fitted Curve
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
.fallback_markers_from_fit <- function (bathy, peak_value, trough_value,
                                        acrophase_time, newt, fit_pred) {
    if (any (is.na (bathy) || is.na (peak_value) || is.na (trough_value))) {
        M1 <- which.max (fit_pred)
        m1 <- which.min (fit_pred)
        bathy <- ifelse (is.na (bathy), newt [m1], bathy)
        peak_value <- ifelse (is.na (peak_value), fit_pred [M1], peak_value)
        trough_value <- ifelse (is.na (trough_value), fit_pred [m1],
            trough_value
        )
        if ((length (acrophase_time) == 0 || is.na (acrophase_time [1]))) {
            acrophase_time <- newt [M1]
        }
    }
    list (
        bathy = bathy, peak_value = peak_value, trough_value = trough_value,
        acrophase_time = acrophase_time
    )
}

#' @title Add Model Fit Line and Confidence Interval
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
.add_model_fit_and_ci <- function (g, newt, fit_pred, ci, ym, yM) {
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

#' @title Add MESOR Line and Data Points
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
.add_mesor_and_points <- function (g, mesor, aug, point_size, acrophase_time,
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

#' @title Add Acrophase Vertical Lines
#' @description
#' Adds vertical line(s) at the acrophase time(s) to a ggplot object.
#' @param g A ggplot object.
#' @param acrophase_time Acrophase time value(s).
#' @param aug Data frame with observed data.
#' @returns Updated ggplot object.
#' @noRd
.add_acrophase_verticals <- function (g, acrophase_time, aug) {
    y_sf <- floor (max (aug$y_obs, na.rm = TRUE) * 0.8)
    Ay <- seq_len (y_sf)
    Ax <- rep (acrophase_time, each = length (Ay))

    g + ggplot2::geom_line (
        ggplot2::aes (x = Ax, y = Ay, colour = "Acrophase"),
        linewidth = 0.9,
        inherit.aes = FALSE
    )
}

#' @title Add Inactive Period Rectangles
#' @description
#' Identifies and adds shaded rectangles for detected inactive periods
#' to a ggplot object.
#' @param g A ggplot object.
#' @param aug Data frame with observed data.
#' @returns Updated ggplot object.
#' @noRd
.add_inactive_periods <- function (g, aug) {
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

#' @title Add Amplitude Annotation Segments
#' @description
#' Adds dashed line segments to visualize the amplitude from MESOR to
#' peak value at acrophase time(s).
#' @param g A ggplot object.
#' @param amplitude Amplitude value(s).
#' @param acrophase_time Acrophase time value(s).
#' @param mesor MESOR value.
#' @returns Updated ggplot object.
#' @noRd
.add_amplitude_segments <- function (g, amplitude, acrophase_time, mesor) {
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

#' @title Add Parameter Labels
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
.add_labels <- function (g, labels, use_posthoc, tau, mesor, amplitude,
                         acrophase_time, aug) {
    if (!labels) {
        return (g)
    }

    Tl <- if (use_posthoc) "post.hoc" else tau
    Cof.Names <- c (
        "MESOR", paste0 ("Amplitude.", Tl),
        paste0 ("Acrophase.", Tl)
    )
    Value <- c (mesor, amplitude [1], acrophase_time [1])
    xAxis <- c (
        min (aug$t_obs, na.rm = TRUE), acrophase_time [1],
        acrophase_time [1]
    )
    yAxis <- c (
        mesor, (mesor + amplitude [1]) * 4 / 5,
        mesor + amplitude [1]
    )

    g + ggrepel::geom_label_repel (
        ggplot2::aes (
            x = xAxis, y = yAxis,
            label = paste0 (Cof.Names, " = ", round (Value, 2))
        ),
        label.size = NA, label.r = 0.25, label.padding = 0.25,
        force = 10, segment.color = "transparent", fontface = "bold",
        inherit.aes = FALSE
    )
}

#' @title Add Plot Scales, Theme, and Labels
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
.add_scales_and_theme <- function (g, legend.position, tau, object,
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
                "MESOR"     = "red",
                "Observed"  = "black",
                "Acrophase" = "purple",
                "Peak"      = "black",
                "Trough"    = "black"
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

    essential_tau <- if (length (tau) == 1) {
        as.character (tau)
    } else {
        paste (tau, collapse = ",")
    }
    method_lbl <- if (!is.null (object$method)) {
        object$method
    } else
    if (inherits (object, "CosinorM.KDE")) "KDE" else "Parametric"
    essential <- paste0 ("(", essential_tau, "Hour) - ", method_lbl)
    parts <- c (class (object) [1])
    if (!is.null (title_extra) && nzchar (title_extra)) {
        parts <- c (parts, title_extra)
    }
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
                margin = ggplot2::margin (b = 15)
            ),
            legend.margin = ggplot2::margin (0, 0, 0, 0)
        ) +
        ggplot2::labs (x = "Time", y = "Activity", title = plot_title) +
        ggplot2::scale_x_continuous (
            breaks = seq (0, day, by = 6),
            expand = c (0, 0)
        ) +
        ggplot2::coord_cartesian (xlim = c (0, day))
}
