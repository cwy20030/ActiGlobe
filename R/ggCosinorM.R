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
#' Create visualization of a CosinorM or CosinorM.KDE fit using ggplot2. The plot shows the parametric cosinor fit over a fine time grid, optional pointwise confidence bands, observed data points, MESOR line, acrophase verticals, amplitude annotation segments, and labelled parameter values when requested.
#'
#' @import stats ggplot2 ggrepel viridis
#' @importFrom scales alpha
#' @param object A fitted model of class \code{\link{CosinorM}} or \code{\link{CosinorM.KDE}}
#' @param labels Logical; Default `TRUE` places repelled labels on the plot with MESOR, amplitude(s), and acrophase(s).
#' @param ci Logical; Default `TRUE` computes and draws pointwise parametric confidence bands for the fitted cosinor curve using the model covariance.
#' @param ci_level Confidence level for the pointwise bands, expressed in numeric value between 0 and 1.
#' @param n Integer; number of points on the fine prediction grid used to draw the fitted cosinor and confidence ribbon. (default to `400`)
#' @param point_size Numeric; plotting size for observed points. (default: `0.5`)
#' @param title_extra Optional character string appended to the plot title for extra context.
#' @param legend.position Position of the legend on the plot; default is `"right"`. Other options include `"top"`, `"bottom"`, `"left"`, or a numeric vector of length two specifying x and y coordinates.
#' @param ... Additional arguments (currently ignored) kept for future update
#'
#' @return A `ggplot` object representing the cosinor model fit visualization.
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
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggrepel]{geom_label_repel}}, \code{\link[stats]{predict}}
#' @keywords plot ggplot graph cosinor
#' @export

ggCosinorM <- function (object, labels = TRUE, ci = TRUE, ci_level = 0.95,
                        n = 400, point_size = 0.5, title_extra = NULL, legend.position = "right",
                        ...) {

    # Accept both parametric CosinorM and KDE-based CosinorM.KDE ----------
    if (!inherits (object, c ("CosinorM", "CosinorM.KDE"))) {
        stop ("ggCosinorM: object must be a CosinorM or CosinorM.KDE fit.", call. = FALSE)
    }


    # Check Essential Parameters ----------------------
    day <- 24

    tau <- if (!is.null (object$tau)) object$tau else 24
    nT <- length (tau)

    # Observed data --------------
    if (!is.null (object$model) && !is.null (object$model$time)) {
        t_obs <- object$model$time
        y_obs <- if (!is.null (object$model$activity)) {
            object$model$activity
        } else {
            stats::model.response (stats::model.frame (object))
        }
    } else {
        stop ("Object missing model$time/activity for observed data.", call. = FALSE)
    }
    aug <- data.frame (t_obs = t_obs, y_obs = y_obs)

    # Parameter source selection -----------------
    coef_cos <- object$coef.cosinor
    post <- object$post.hoc

    use_parametric_single <- inherits (object, "CosinorM") && nT == 1
    use_posthoc <- !use_parametric_single

    # MESOR ------------------
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["MESOR.ph"])) stop ("Post-hoc MESOR.ph is required.", call. = FALSE)
        mesor <- as.numeric (post ["MESOR.ph"])
    } else {
        if (is.null (coef_cos) || is.null (coef_cos ["MESOR"])) stop ("Parametric MESOR is required.", call. = FALSE)
        mesor <- as.numeric (coef_cos ["MESOR"])
    }

    # Amplitude -----------
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Amplitude.ph"])) stop ("Post-hoc Amplitude.ph is required.", call. = FALSE)
        amplitude <- as.numeric (post ["Amplitude.ph"])
    } else {
        amp_name <- paste0 ("Amplitude.", tau)
        if (!all (amp_name %in% names (coef_cos))) stop ("Parametric amplitude not found.", call. = FALSE)
        amplitude <- as.numeric (coef_cos [amp_name])
    }

    # Acrophase (time in hours) -------------
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Acrophase.ph.time"])) stop ("Post-hoc Acrophase.ph.time is required.", call. = FALSE)
        acrophase_time <- as.numeric (post ["Acrophase.ph.time"])
    } else {
        phi_name <- paste0 ("Acrophase.", tau)
        if (!all (phi_name %in% names (coef_cos))) stop ("Parametric acrophase not found.", call. = FALSE)
        acrophase_rad <- as.numeric (coef_cos [phi_name])
        acrophase_time <- ((acrophase_rad * tau / (2 * pi)) %% tau)
    }

    # Bathy (trough time in hours) ---------------
    if (use_posthoc) {
        if (is.null (post) || is.null (post ["Bathyphase.ph.time"])) {
            bathy <- NA_real_
        } else {
            bathy <- rep (as.numeric (post ["Bathyphase.ph.time"]))
        }
    } else {
        bathy <- (acrophase_time - tau / 2) %% day
    }

    # Peak/trough values -------------
    if (use_posthoc) {
        peak_value <- if (!is.null (post ["Peak.ph"])) as.numeric (post ["Peak.ph"]) else NA_real_
        trough_value <- if (!is.null (post ["Trough.ph"])) as.numeric (post ["Trough.ph"]) else NA_real_
    } else {
        peak_value <- mesor + amplitude
        trough_value <- mesor - amplitude
    }

    # Fitted curve source selection -----------------
    uses_kdf <- FALSE
    if (inherits (object, "CosinorM.KDE") && !is.null (object$kdf)) {
        # Use observation-side fitted values (requested)
        kdf <- object$kdf
        newt <- t_obs
        fit_pred <- kdf$fitted.values
        se_fit <- if (!is.null (kdf$fitted.se)) kdf$fitted.se else rep (NA_real_, length (fit_pred))
        uses_kdf <- TRUE
    } else if (inherits (object, "CosinorM.KDE") && !is.null (object$grid)) {
        # Fallback to grid if kdf missing
        gtheta <- object$grid$theta
        newt <- (gtheta * tau) / (2 * pi)
        fit_pred <- object$grid$fitted.values
        se_fit <- object$grid$fitted.se
        uses_kdf <- TRUE
    } else {
        # Parametric cosinor
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
    }

    # CI band -----------
    if (ci) {
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
    } else {
        ym <- yM <- rep (NA_real_, length (fit_pred))
    }

    # --- Fallback markers from fitted curve if post.hoc incomplete ---
    if (any (is.na (bathy) || is.na (peak_value) || is.na (trough_value))) {
        M1 <- which.max (fit_pred)
        m1 <- which.min (fit_pred)
        bathy <- ifelse (is.na (bathy), newt [m1], bathy)
        peak_value <- ifelse (is.na (peak_value), fit_pred [M1], peak_value)
        trough_value <- ifelse (is.na (trough_value), fit_pred [m1], trough_value)
        if ((length (acrophase_time) == 0 || is.na (acrophase_time [1]))) {
            acrophase_time <- newt [M1]
        }
    }

    # Plot ---
    g <- ggplot2::ggplot () +
        # Model fit line (mapped to legend)
        ggplot2::geom_line (
            ggplot2::aes (x = newt, y = fit_pred, colour = "Model Fit"),
            size = 0.9,
            inherit.aes = FALSE
        )

    # ribbon (confidence band) mapped to legend
    if (ci && any (is.finite (ym) & is.finite (yM))) {
        g <- g + ggplot2::geom_ribbon (
            ggplot2::aes (x = newt, ymin = ym, ymax = yM, fill = "Fit CI"),
            colour = NA,
            alpha = 0.18,
            inherit.aes = FALSE
        )
    }

    #### MESOR line, observed points, peak/trough points -----
    g <- g +
        # MESOR horizontal line mapped to legend
        ggplot2::geom_hline (
            ggplot2::aes (yintercept = mesor, colour = "MESOR"),
            size = 0.9,
            inherit.aes = FALSE
        ) +
        # Observed points (from aug) mapped to legend
        ggplot2::geom_point (
            data = aug,
            ggplot2::aes (x = t_obs, y = y_obs, colour = "Observed"),
            alpha = 0.5,
            size = point_size,
            inherit.aes = FALSE
        ) +
        # Peak and trough points with distinct shapes and legend labels
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

    #### Acrophase verticals ---------------
    y_sf <- floor (max (aug$y_obs, na.rm = TRUE) * 0.8)
    Ay <- seq_len (y_sf)
    Ax <- rep (acrophase_time, each = length (Ay))

    g <- g + ggplot2::geom_line (
        ggplot2::aes (x = Ax, y = Ay, colour = "Acrophase"),
        size = 0.9,
        inherit.aes = FALSE
    )

    # inactive periods ---------------
    inatv <- Prob.Inact (
        y = aug$y_obs,
        T = aug$t_obs,
        k = 12,
        threshold = 3,
        logical = FALSE
    )

    if (!nrow (inatv) == 0) {
        Ymin <- rep (0, nrow (inatv))
        Ymax <- rep (y_sf, nrow (inatv))
        start <- inatv$start
        end <- inatv$end

        g <- g +
            ggplot2::geom_rect (
                ggplot2::aes (
                    xmin = start, xmax = end,
                    ymin = Ymin, ymax = Ymax,
                    fill = "Inactive Period"
                ),
                inherit.aes = FALSE,
                alpha = 0.1
            )
    }

    # amplitude segments (keep them out of legend or map if desired)
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

    # labels (unchanged)
    if (labels) {
        Tl <- if (use_posthoc) "post.hoc" else tau
        Cof.Names <- c ("MESOR", paste0 ("Amplitude.", Tl), paste0 ("Acrophase.", Tl))
        Value <- c (mesor, amplitude [1], acrophase_time [1])
        xAxis <- c (min (aug$t_obs, na.rm = TRUE), acrophase_time [1], acrophase_time [1])
        yAxis <- c (mesor, (mesor + amplitude [1]) * 4 / 5, mesor + amplitude [1])
        g <- g + ggrepel::geom_label_repel (
            ggplot2::aes (x = xAxis, y = yAxis, label = paste0 (Cof.Names, " = ", round (Value, 2))),
            label.size = NA, label.r = 0.25, label.padding = 0.25,
            force = 10, segment.color = "transparent", fontface = "bold",
            inherit.aes = FALSE
        )
    }

    # legends ------------------------------
    # Scales for legend entries: colours, fills and shapes
    legend_keys <- c (
        "Observed", "Peak", "Trough", "Model Fit",
        "MESOR", "Acrophase"
    )


    g <- g +
        # keep the same name for all scales so they can be merged
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


    # title and axes ---------------------
    essential_tau <- if (length (tau) == 1) as.character (tau) else paste (tau, collapse = ",")
    method_lbl <- if (!is.null (object$method)) object$method else if (inherits (object, "CosinorM.KDE")) "KDE" else "Parametric"
    essential <- paste0 ("(", essential_tau, "Hour) - ", method_lbl)
    parts <- c (class (object) [1])
    if (!is.null (title_extra) && nzchar (title_extra)) parts <- c (parts, title_extra)
    parts <- c (parts, essential)
    plot_title <- paste (parts, collapse = " ")

    g <- g + ggplot2::theme_minimal () +
        ggplot2::theme (
            legend.position = legend.position,
            panel.grid.major = ggplot2::element_blank (),
            panel.grid.minor = ggplot2::element_blank (),
            plot.title = ggplot2::element_text (hjust = 0.5, face = "bold")
        ) +
        ggplot2::labs (x = "Time", y = "Activity", title = plot_title) +
        ggplot2::theme (
            plot.title = ggplot2::element_text (size = 16, margin = ggplot2::margin (b = 15)),
            legend.margin = unit (0, "cm")
        ) +
        ggplot2::scale_x_continuous (breaks = seq (0, day, by = 6), expand = c (0, 0)) +
        ggplot2::coord_cartesian (xlim = c (0, day))

    return (g)


}
