#  File ActiGlobe/R/ggCosinorM.R
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
#
#
#' @title Plot `CosinorM` Fit with `ggplot2`
#'
#' @description
#' Create visualization of a CosinorM model fit using ggplot2. The plot shows the parametric cosinor fit over a fine time grid, optional pointwise confidence bands, observed data points, MESOR line, acrophase verticals, amplitude annotation segments, and labelled parameter values when requested.
#'
#' @import stats ggplot2 ggrepel viridis
#' @param object A fitted model of class [CosinorM()]
#' @param labels Logical; Default `TRUE` places repelled labels on the plot with MESOR, amplitude(s), and acrophase(s).
#' @param ci Logical; Default `TRUE` computes and draws pointwise parametric confidence bands for the fitted cosinor curve using the model covariance.
#' @param ci_level Confidence level for the pointwise bands, expressed in numeric value between 0 and 1.
#' @param n Integer; number of points on the fine prediction grid used to draw the fitted cosinor and confidence ribbon. (default to `400`)
#' @param point_size Numeric; plotting size for observed points. (default: `0.5`)
#' @param title_extra Optional character string appended to the plot title for extra context.
#' @param ... Additional arguments (currently ignored) kept for generic compatibility.
#'
#' @examples
#' \dontrun{
#' # Import data
#' FlyEast
#'
#' BdfList =
#' BriefSum(df = FlyEast ,
#'          SR = 1/60,
#'          Start = "2017-10-24 13:45:00")
#'
#' # Let's extract actigraphy data from a single day
#' df <- BdfList$df
#' df <- subset(df, df$Date == "2017-10-27")
#'
#' fit <- CosinorM(time = df$Time,
#'                 activity = df$Activity,
#'                 tau = 24,
#'                 method = "OLS")
#'
#'
#' p <- ggCosinorM(fit,
#'                 labels = TRUE,
#'                 ci = TRUE,
#'                 ci_level = 0.95,
#'                 title_extra = "2017-10-24")
#' print(p)
#' }
#'
#' @seealso
#' [ggplot2::ggplot()], [ggrepel::geom_label_repel()], [stats::predict()]
#'
#' @export


ggCosinorM <- function(object, labels = TRUE, ci = TRUE, ci_level = 0.95, n = 400, point_size = 0.5, title_extra = NULL,...) {
  if (!inherits(object, "CosinorM")) stop("ggcosinor: object must be a CosinorM fit (class c('CosinorM','lm')).", call. = FALSE)


  tau <- object$tau
  p <- length(tau)

  coef_cos <- object$coef.cosinor
  if (is.null(coef_cos)) stop("CosinorM object missing coef.cosinor element.", call. = FALSE)

  mesor <- as.numeric(coef_cos["MESOR"])
  amp_names <- paste0("Amplitude.", tau)
  phi_names <- paste0("Acrophase.", tau)
  amplitude <- as.numeric(coef_cos[amp_names])
  acrophase_rad <- as.numeric(coef_cos[phi_names])
  acrophase_time <- (acrophase_rad * tau) / (2 * pi)
  acrophase_time <- (acrophase_time %% tau)

  peak_value <- mesor + amplitude
  trough_value <- mesor - amplitude

  # observed data
  t_obs <- object$time
  mf <- stats::model.frame(object)
  y_obs <- stats::model.response(mf)
  fitted_vals <- as.numeric(stats::predict(object))
  resid_vals <- as.numeric(residuals(object))
  aug <- data.frame(t_obs = t_obs, y_obs = y_obs, .fitted = fitted_vals, .resid = resid_vals)

  t_min <- min(aug$t_obs, na.rm = TRUE)
  t_max <- max(aug$t_obs, na.rm = TRUE)

  # prediction grid (fine)
  newt <- seq(t_min, t_max, length.out = n)
  # construct design matrix columns matching CosinorM (C1,S1,C2,S2,...)
  newdata <- data.frame(t_obs = newt)
  for (i in seq_len(p)) {
    newdata[[paste0("C", i)]] <- cos(2 * pi * newt / tau[i])
    newdata[[paste0("S", i)]] <- sin(2 * pi * newt / tau[i])
  }

  # Build model matrix for newdata consistent with lm's terms
  intercept_col <- rep(1, nrow(newdata))
  X_new <- as.matrix(cbind("(Intercept)" = intercept_col, newdata[ , grepl("^C|^S", names(newdata)) , drop = FALSE]))

  # obtain coefficient vector in same order as X_new columns
  coef_lm <- stats::coef(object)
  expected_names <- colnames(X_new)
  coef_vec <- numeric(length(expected_names))
  names(coef_vec) <- expected_names
  common <- intersect(names(coef_lm), expected_names)
  coef_vec[common] <- coef_lm[common]

  fit_pred <- as.numeric(X_new %*% coef_vec)

  # compute pointwise standard errors using model covariance
  vcov_mat <- if (!is.null(object$vcov)) object$vcov else stats::vcov(object)
  if (!all(colnames(vcov_mat) %in% names(coef_vec))) {
    full_vcov <- matrix(0, nrow = length(coef_vec), ncol = length(coef_vec), dimnames = list(names(coef_vec), names(coef_vec)))
    common_v <- intersect(rownames(vcov_mat), rownames(full_vcov))
    full_vcov[common_v, common_v] <- vcov_mat[common_v, common_v]
    vcov_mat <- full_vcov
  } else {
    vcov_mat <- vcov_mat[names(coef_vec), names(coef_vec)]
  }
  se_fit <- sqrt(rowSums((X_new %*% vcov_mat) * X_new))




    alpha <- 1 - ci_level
    tcrit <- stats::qt(1 - alpha / 2, df = stats::df.residual(object))


    ym <- ifelse(ci, fit_pred - tcrit * se_fit, NA)
    yM <- ifelse(ci, fit_pred + tcrit * se_fit, NA)


  # annotation layers
  amp_layers <- lapply(seq_len(p), function(i) {
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = acrophase_time[i],
                             xend = acrophase_time[i],
                             y = mesor,
                             yend = mesor + amplitude[i]),
      linetype = "twodash", lineend = "butt", linejoin = "mitre"
    )
  })
  acro_horiz <- lapply(seq_len(p), function(i) {
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = 0,
                             xend = acrophase_time[i],
                             y = mesor + amplitude[i],
                             yend = mesor + amplitude[i]),
      linetype = "twodash", lineend = "butt", linejoin = "mitre"
    )
  })



  # Build plot: simple equal-sized points for observed data
  # Build plot: simple equal-sized points for observed data
  g <- ggplot2::ggplot() +
    # fitted cosinor line
    ggplot2::geom_line(mapping = ggplot2::aes(x = newt, y = fit_pred), color = "blue", size = 0.9)


  if (ci) {
    g <- g +
      # CI ribbon (parametric) if requested
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = t_obs, ymin = ym, ymax = yM), colour = "cyan", alpha = 0.18, inherit.aes = FALSE)
  } else {



  }

    # horizontal MESOR
  g <- g +
    ggplot2::geom_hline(yintercept = mesor, color = "red", size = 0.9)
    # verticals for acrophases
  g <- g +
    ggplot2::geom_vline(xintercept = acrophase_time, color = "purple", size = 0.9)

    # observed points: fixed size, not encoded by residual
  g <- g +
    ggplot2::geom_point(data = aug, mapping = ggplot2::aes(x = t_obs, y = y_obs), alpha = 0.5, size = point_size) +
    viridis::scale_color_viridis(option = "magma") # kept for compatibility if user adds color aesthetic





  # add annotation layers
  for (l in amp_layers) g <- g + l
  for (l in acro_horiz) g <- g + l

  # peaks
  Atp = acrophase_time
  pVp = peak_value
  # troughs
  Att = (acrophase_time + tau / 2) %% tau
  pVt = trough_value

  g <- g +
    ggplot2::geom_point(mapping = ggplot2::aes(x = Atp, y = pVp), shape = 18, size = 3)

  g <- g +
    ggplot2::geom_point(mapping = ggplot2::aes(x = Att, y = pVt), shape = 18, size = 3)

  if (labels) {
    Cof.Names = c("MESOR", paste0("Amplitude.", tau), paste0("Acrophase.", tau))
    Value = c(mesor, amplitude, acrophase_time)
    xAxis = c(t_min, acrophase_time, acrophase_time)
    yAxis = c(mesor, mesor + amplitude / 2, mesor + amplitude)

    g <- g + ggrepel::geom_label_repel(
      mapping = ggplot2::aes(x = xAxis, y = yAxis, label = paste0(Cof.Names, " = ", round(Value, 1))),
      label.size = NA, label.r = 0.25, label.padding = 0.25,
      force = 10, segment.color = "transparent",
      fontface = "bold"
    )
  }

  # Build title: essential = paste0(tau, "Hr - ", method)
  essential_tau <- if (length(tau) == 1) as.character(tau) else paste(tau, collapse = ",")
  essential <- paste0(essential_tau, "Hr - ", if (!is.null(object$method)) object$method else "")
  parts <- c(class(object)[1])
  if (!is.null(title_extra) && nzchar(title_extra)) {
    parts <- c(parts, title_extra)
  }
  parts <- c(parts, essential)
  plot_title <- paste(parts, collapse = " ")

  g <- g + ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(x = "time", y = "activity", title = plot_title)

  return(g)
}
