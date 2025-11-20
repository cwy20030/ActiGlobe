#  File ActiGlobe/R/boot.seci.R
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
#' @title Bootstrap Standard Errors and Confidence Intervals for Model Parameters
#' @description Computes standard errors and confidence intervals for cosinor and post-hoc parameters via non-parametric bootstrap
#' @import stats
#'
#' @param object a fitted `CosinorM` or `CosinorM.KDE` model object.
#' @param level Numeric scaler. the confidence level.
#' @param N Numeric scaler. Numbers of bootstraps required to estimate the standard errors and confidence intervals. Default: 500
#' @param digits Numeric scaler. Integer indicating the number of decimal places (round) to be used. Default: 2
#'
#'
#' @returns
#' A data.frame with one row per cosinor coefficient and columns:
#' \itemize{
#'   \item Estimate: Mean of bootstrap coefficient values.
#'   \item Std Error: Bootstrap standard deviation of each coefficient across N resamples.
#'   \item t value: Ratio of the observed estimate to its bootstrap standard error, analogous to a
#'     signal-to-noise measure: \deqn{t = \hat{\theta}_obs / SE_boot}
#'   \item lower CI label: Percentile lower bound at \eqn{\frac{\alpha}{2}},
#'      where \eqn{\alpha = 1 - level}.
#'   \item upper CI label: Percentile upper bound at \eqn{1 - \frac{\alpha}{2}}.
#' }
#'
#'
#' @seealso \code{\link[boot]{boot}}
#' @examples
#'
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
#' # Multicomponent Cosinor Model
#' fit <- Cosinor(time = df$Time,
#'                activity = df$Activity,
#'                tau = c(12,24),
#'                method = "OLS")
#'
#' # inspect coefficients
#'
#' boot.seci(object = fit,
#'           level = 0.95,
#'           N = 500)
#'
#'
#' # Gaussian Kernel Density Estimation
#' fit2 <- CosinorM.KDE(time = df$Time,
#'                      activity = df$Activity)
#'
#' # inspect coefficients
#'
#' boot.seci(object = fit2,
#'           level = 0.95,
#'           N = 500)
#' }
#' @keywords boot bootstrap ci se
#' @export
#'

boot.seci <- function(object, level = 0.95, N = 500, digits = 2){

  ## Checkpoint -----------------------
  if ("param" %in% names(object)) stop("Object cannot be a diluted model structure.")

  ## Set seed  -----------
  set.seed(123456789)

  ## Alpha value ---------------
  a <- 1 - level
  a.half <- (a / 2)
  da <- 1 - a.half
  CIs <- c(sprintf("%.1f%%", 100 * a.half),
           sprintf("%.1f%%", 100 * da))

  ## Extract parameters  -----------
  activity <- object$model$activity
  time     <- object$model$time
  arctan2  <- object$arctan2
  Coefs    <- c(object$coef.cosinor, object$post.hoc)

  ## Output data.frame -----------------------
  boot.df <- data.frame(matrix(nrow = N, ncol = length(Coefs)))
  names(boot.df) <- names(Coefs)


  if (inherits(object, "CosinorM.KDE")) {
    bw   <- object$bw
    grid <- object$grid.size

    for (b in seq_len(N)) {

      idx <- sample.int(length(time), size = length(time), replace = TRUE)
      idx <- idx[order(idx)]
      Time <- as.numeric(time[idx])
      Act  <- as.numeric(activity[idx])

      mdl <- CosinorM.KDE(time = Time,
                             activity = Act,
                             bw = bw,
                             grid = grid,
                             arctan2 = arctan2,
                             dilute = TRUE)

      boot.df[b,] <- mdl$coef.cosinor
    }
  }




  if (inherits(object, "CosinorM")) {
    tau    <- object$tau
    method <- object$method
    type   <- object$type


    for (b in seq_len(N)) {
      idx <- sample.int(length(time), size = length(time), replace = TRUE)
      idx <- idx[order(idx)]
      mdl <- CosinorM(time = time[idx],
                         activity = activity[idx],
                         tau = tau,
                         method = method,
                         type = type,
                         dilute = TRUE)

      boot.df[b,] <- mdl$coef.cosinor
    }
  }

  Est <- unlist(lapply(boot.df, mean, na.rm = T))
  SEs <- unlist(lapply(boot.df, sd, na.rm = T))
  lci <- unlist(lapply(boot.df, quantile, 0.025))
  uci <- unlist(lapply(boot.df, quantile, 0.975))



  ## t-values (observed / SE)
  t_vals <- Coefs / SEs


  ## Combine results
  Out <- data.frame(
    Estimate = Est,
    SE = SEs,
    t_value = t_vals,
    lci = lci,
    uci = uci,
    row.names = names(Coefs)
  )
  names(Out) <- c("Estimate", "Std. Error", "t value",  CIs)

  Out <- round(x = Out,
               digits = digits)

  return(Out)

}

