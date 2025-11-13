#  File ActiGlobe/R/y_hat_se.R
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
#' @title y_hat_se
#' @import stats
#' @param object A fitted model of class \code{\link{CosinorM}}
#' @param df A data.frame; The
#' @noRd


y_hat_se <- function(object, n = 400, df, alpha = 0.05){

  # observed data ---------------
  t_obs       <- object$time
  tau         <- object$tau
  nT          <- length(tau)
  mf          <- stats::model.frame(object)
  y_obs       <- stats::model.response(mf)
  fitted_vals <- as.numeric(stats::predict(object))
  resid_vals  <- as.numeric(residuals(object))
  aug         <- data.frame(t_obs = t_obs, y_obs = y_obs, .fitted = fitted_vals, .resid = resid_vals)

  t_min <- min(aug$t_obs, na.rm = TRUE)
  t_max <- max(aug$t_obs, na.rm = TRUE)

  # prediction grid (fine)
  newt <- seq(t_min, t_max, length.out = n)
  # construct design matrix columns matching CosinorM (C1,S1,C2,S2,...)
  newdata <- data.frame(t_obs = newt)

  for (i in seq_len(nT)) {
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

  if (ci) {
    ym <-  fit_pred - tcrit * se_fit
    yM <-  fit_pred + tcrit * se_fit

  } else {
    ym <- NA
    yM <- NA

  }


}


