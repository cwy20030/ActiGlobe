#  File ActiGlobe/R/CosinorM.R
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
#' @title `Cosinor` Model
#'
#' @description
#' Fit a cosine-based harmonic linear regression on circular time (hours of day)
#'
#'
#' @details
#' The `Cosinor` model is a cosine-based harmonic regression used to estimate circadian rhythm parameters.
#'
#' \strong{Single-phase equation}:
#'       \deqn{ y = M + A \cos \left(\frac{2\pi t}{\tau} + \phi\right) }
#' \itemize{
#'   \item \eqn{ M }: MESOR (mid-line estimating statistic of rhythm), the intercept
#'   \item \eqn{ A }: Amplitude, peak deviation from M
#'   \item \eqn{ t }: Time coordinate within the cycle
#'   \item \eqn{ \tau }: The assumed period length
#'   \item \eqn{ \phi }: Acrophase (time-of-peak), computed from fitted sine and cosine coefficients
#' }
#'
#' \strong{Linearized form}:
#'      \deqn{ \hat{y} = M + \beta x + \gamma z + \epsilon }
#' \itemize{
#'   \item \eqn{\beta = A * cos(\phi)}, estimated coefficient for the cosine term
#'   \item \eqn{x = cos(\frac{2\pi t} {\tau})}, cosine-transformed time
#'   \item \eqn{\gamma = -A * sin(\phi)}, estimated coefficient for the sine term
#'   \item \eqn{z = sin(\frac{2\pi t} {\tau})}, sine-transformed time
#'   \item \eqn{ \epsilon }: error term
#' }
#'
#' Model parameters are estimated by minimizing the residual sum of squares:
#' \deqn{RSS = \sum_{i=1}^n (y_i - (M + \beta x_i + \gamma z_i))^2}
#' By default, \code{\link[stats]{lm}} fits this via QR decomposition.
#'
#' @details
#' \strong{Acrophase \eqn{ (\phi) } interpretation}:
#'  \eqn{ \phi } is derived from
#'  \deqn{ \phi = atan2(-\gamma, \beta) }
#'  Note, \eqn{ \phi } is converted to clock time to identify the peak activity time.
#'
#' \strong{Amplitude \eqn{ (A) } estimation}:
#' Amplitude is calculated from fitted sine and cosine coefficients as:
#' \deqn{A= \sqrt {(\beta^2 + \gamma^2)}}
#'
#' @import stats sandwich
#' @param time Numeric vector of time coordinates for each data point
#' @param activity Numeric vector of activity counts from an actigraphy device
#' @param tau Numeric scalar or vector for the assumed circadian period. Default is 24 for single-phase; multiple phases can be supplied via [c()]
#'
#' @param method Character string specifying estimation method
#' \itemize{
#'   \item "OLS": Ordinary least squares via \code{\link[stats]{lm}} (default)
#'   \item "FGLS": Feasible generalized least squares; models heteroskedasticity via a log-variance fit to squared OLS residuals, computes weights, and refits by weighted least squares
#' }
#'
#' @param type Character string passed to \code{\link[sandwich]{vcovHC}} for robust standard error computation
#' @returns
#' A list of class c("CosinorM", "lm") containing:
#' \itemize{
#'   \item tau: The assumed period length
#'   \item time: The time coordinates of the recording
#'   \item method: The estimation method used
#'   \item coef.cosinor: Named numeric vector with entries:
#'     \itemize{
#'       \item MESOR: Mid-line estimating statistic of rhythm
#'       \item Amplitude: Predicted peak activity magnitude
#'       \item Acrophase: Acrophase in radian (time-of-peak)
#'       \item Beta: the coefficient of the cosine function
#'       \item Gamma: the coefficient of the sine function
#'     }
#'   \item vcov: Robust variance-covariance matrix
#'   \item se: Standard errors
#' }
#' Inherits all components from stats::lm.
#'
#' @references
#' Chambers, J. M. (1992) Linear models. Chapter 4 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
#'
#' Wilkinson, G. N. and Rogers, C. E. (1973). Symbolic descriptions of factorial models for analysis of variance. Applied Statistics, 22, 392-399. doi:10.2307/2346786.
#'
#' Harvey, A. C. (1976). Estimating Regression Models with Multiplicative Heteroscedasticity. Econometrica, 44(3), 461-465. doi:10.2307/1913974
#'
#' @seealso
#' \code{\link[stats]{lm}} for general linear model.
#'
#' `CosinorM.KD` for Gaussian.
#' @examples
#' #' require(stats)
#' require(graphics)
#'
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
#' fit <- CosinorM(time = df$Time,
#'                 activity = df$Activity,
#'                 tau = 24,
#'                 method = "OLS")
#'
#'
#' # inspect coefficients
#' fit$coef.cosinor
#'
#'
#' # plot KDE in hours
#' plot(fit$time, fit$fitted.values, type = "l", xlab = "Hour", ylab = "KDE")
#' }
#' @keywords cosinor
#' @export



CosinorM <- function(time, activity, tau, method = "OLS", type = "HC3") {

  # Check the variable class
  if (!inherits(activity,"numeric")) activity = as.numeric(as.character(activity))

  if (!inherits(time,"numeric")) time = C2T(time)
  if (any(time > 24 | time < 0)) stop("Currently, the model cannot fit actigraphy recordings lasting longer than a day.
                                       Please, rescale the time coordinate to between 0 and 24.
                                       Note that it is crucial to have the proper time coordinate since the model relies on it.")


  # Number of assumed rhythms
  nT <- length(tau)
  vars <- as.vector(outer(c("C", "S"), 1:nT, paste0)) ### C = x and S = z in the linear equation

  # Build cosine and sine columns for each period
  ## Create a model data.frame
  Mdf <- data.frame(matrix(nrow = length(time), ncol = nT * 2))
  names(Mdf) = vars

  for (i in 1:nT) {
    Mdf[[paste0("C",i)]] = cos(2 * pi * time / tau[i])
    Mdf[[paste0("S",i)]] = sin(2 * pi * time / tau[i])
  }

  # Assemble data frame and formula
  fm <- as.formula(paste("activity ~", paste0(vars, collapse = " + ")))
  df <- data.frame(activity = activity, Mdf)

  # Fit linear model
  model <- stats::lm(fm, data = df)

  # Case Switch Between OLS and FGLS.
  ## Feasible GLS via weighted least square with log-variance https://stats.stackexchange.com/questions/97437/feasible-generalized-least-square-in-r
  if (method == "FGLS") {
    #  Step 0: Keep the OLS model.
    model_ols <- model

    ## Step 1: Fit a variance model (log-variance)
    df$e2 <- resid(model)^2 ## Squared residual
    fm2 <- as.formula(paste("log(e2) ~", paste0(vars, collapse = " + "))) ### Create a log-variance model
    varmod <- lm(fm2, data = df)

    # Step 2: compute weight based on the log-variance model
    df$sigma2_hat = exp(predict(varmod, df))
    w          = 1 / df$sigma2_hat

    # Step 3: Weighted least squares approximates FGLS
    model <- update(model,  data = df, weights = w)
  }


  # Extract and Compute Parameters
  Coef  <- stats::coef(model)

  ## Extract MESOR, beta, gamma
  mesor  <- Coef["(Intercept)"]
  beta   <- Coef[vars[grepl("^C", vars)]]
  gamma  <- Coef[vars[grepl("^S", vars)]]

  ## Compute amp and phi
  amplitude  <- sqrt(beta^2 + gamma^2)
  acrophase  <- atan2(-gamma, beta)


  # Prepare Output
  ## Pack output
  coef_names <- c(
    "MESOR",
    paste0("Amplitude.", tau),
    paste0("Acrophase.", tau),
    paste0("Beta.", tau),
    paste0("Gamma.", tau)
  )
  coefficient <- unname(c(mesor, amplitude, acrophase, beta, gamma))
  names(coefficient) <- coef_names


  ## Inherit the output from lm
  fit <- model
  fit$tau <- tau
  fit$time <- time
  fit$method = method
  fit$coef.cosinor <- coefficient
  fit$type <- type
  fit$vcov <- sandwich::vcovHC(model, type = type)
  fit$se <- sqrt(diag(fit$vcov))

  ## Assign Class
  class(fit) <- c("CosinorM","lm")

  return(fit)

}

