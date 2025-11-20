#  File ActiGlobe/R/CosinorM.R
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
#'       \deqn{ y = M + A \cos \left(\frac{2\pi t}{\tau} - \phi\right) }
#' \itemize{
#'   \item \eqn{ M }: MESOR (mid-line estimating statistic of rhythm), the intercept
#'   \item \eqn{ A }: Amplitude, peak deviation from M
#'   \item \eqn{ t }: Time coordinate within the cycle
#'   \item \eqn{ \tau }: The assumed period length
#'   \item \eqn{ \phi }: Acrophase (time-of-peak), computed from fitted sine and cosine coefficients
#' }
#'
#'
#' Note that because the model is parameterized with a negative phase inside the
#' cosine, this means the the derived peak time from acrophase corresponds to
#' the peak time forward from midnight (not backward). In other words, the
#' when the acrophase is positive, it means the peak activity time occurs after
#' midnight and vice versa.
#'
#'
#' \strong{Linearized form}:
#'      \deqn{ \hat{y} = M + \beta x + \gamma z + \epsilon }
#' \itemize{
#'   \item \eqn{\beta = A * cos(\phi)}, estimated coefficient for the cosine term
#'   \item \eqn{x = cos(\frac{2 * \pi * t} {\tau})}, cosine-transformed time
#'   \item \eqn{\gamma = A * sin(\phi)}, estimated coefficient for the sine term
#'   \item \eqn{z = sin(\frac{2 * \pi * t} {\tau})}, sine-transformed time
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
#'  \deqn{ \phi = \arctan2(\frac{\gamma} {\beta}) }
#'
#'  Note,
#' \itemize{
#'   \item \eqn{ \phi } can be converted to clock time to identify the peak activity time AFTER midnight.
#'   \item if the model is not a mono-phase (i.e., more than one \eqn{ \tau }), there will be \eqn{ \frac{24 (hours)} {\tau} } number of peaks within a day.
#' }
#'
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
#' @param arctan2 Logical; if TRUE (default) acrophase is computed with
#'   \code{atan2(gamma, beta)}, resulting in the quadrant interval between
#'   \eqn{-\pi} and \eqn{\pi}. Whereas, when set to FALSE, the legacy arctangent
#'   quadrant is mapped. The resulting interval lies between \eqn{-\frac{\pi}{2}}
#'   and \eqn{\frac{\pi}{2}}.
#'
#' @param type Character string passed to \code{\link[sandwich]{vcovHC}} for robust standard error computation
#' @param dilute Logical;
#' \itemize{
#'   \item "FALSE": All essential parameters would be produced.  (default)
#'   \item "TRUE": Only cosinor coefficients are returned. This is suited for post-hoc processes, such as computing confidence interval via nonparametric bootstrap
#' }
#'
#'
#' @returns
#' A list of class c("CosinorM", "lm") containing:
#' \itemize{
#'   \item parm: Parameters specified in the model
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
#'   \item post.hoc: Post-hoc peak/trough diagnostics derived from fitted.values at observation angles (MESOR.ph, Bathyphase.ph.time, Trough.ph, Acrophase.ph.time, Peak.ph, Amplitude.ph)
#'   \item extra: only available for ultradian (i.e., \eqn{\tau} less than 24hour) single-component model
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
#' \code{\link[stats]{lm}}  \code{\link{CosinorM.KDE}}
#'
#' @examples
#' require(stats)
#' require(graphics)
#'
#'
#' \dontrun{
#' # Import data
#' data(FlyEast)
#'
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
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
#' # plot Cosinor in hours
#' plot(fit$time, fit$fitted.values, type = "l", xlab = "Hour", ylab = "24-Hour Cosinor Model")
#' }
#' @keywords cosinor
#' @export



CosinorM <- function(time, activity, tau, method = "OLS", arctan2 = TRUE, type = "HC3", dilute = FALSE) {

  # Check the variable class
  if (!inherits(activity,"numeric")) activity = as.numeric(as.character(activity))
  if (all(activity == 0)) stop("all activity values are zero")
  if (any(!is.finite(activity))) stop("activity contains NA/NaN/Inf")
  if (!inherits(time,"numeric")) time = C2T(time)
  if (any(time > 24 | time < 0)) stop("Currently, the model cannot fit actigraphy recordings lasting longer than a day.
                                       Please, rescale the time coordinate to between 0 and 24.
                                       Note that it is crucial to have the proper time coordinate since the model relies on it.")

  # Number of assumed rhythms
  nT <- length(tau)
  vars <- as.vector(outer(c("C", "S"), 1:nT, paste0)) ### C = x and S = z in the linear equation

  day <- 24 ### 24 hours per-day for now
  factor <- day/tau

  dt <- diff(time)
  dt <- dt[dt>0]
  Epc <- 1/min(dt)


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

  # Fit linear model ----------------------------------------
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


  # Extract and Compute Parameters ----------------------------------
  Coef  <- stats::coef(model)

  ## Extract MESOR, beta, gamma
  mesor  <- Coef["(Intercept)"]
  beta   <- Coef[vars[grepl("^C", vars)]]
  gamma  <- Coef[vars[grepl("^S", vars)]]

  ## Compute amplitude and acrophase
  amplitude  <- sqrt(beta^2 + gamma^2)

  if (arctan2) {

    acrophase <- theta <- atan2(gamma , beta)

  } else {

    for (nl in 1:nT) {

      Tacrophase <- acrophase <- theta <- atan(abs(gamma) / beta)

      Bs <- beta[[nl]]
      Gs <- gamma[[nl]]
      acrophase[[nl]] <- ifelse(Bs >= 0 & Gs > 0, -theta,
                                ifelse(Bs < 0 & Gs >= 0, theta - pi,
                                       ifelse(Bs <= 0 & Gs < 0, -theta - pi,
                                              ifelse(Bs > 0 & Gs <= 0, theta - (2 * pi), NA))))

    }

  }




  # Prepare Output
  ## Pack output
  coef_names <- c(
    "MESOR",
    paste0("Amplitude.", tau),
    paste0("Acrophase.", tau),
    paste0("Beta.", tau),
    paste0("Gamma.", tau)
  )



  coef.cosinor <- unname(c(mesor, amplitude, acrophase, beta, gamma))
  names(coef.cosinor) <- coef_names



  ## Compute acrophase time and produce extra parameters ---------------

  ### Extra parameters
  extra <- list()

  if (nT == 1) { ### For single phase cosinor -------------------

    Tacrophase <- ((acrophase *  tau / (2*pi)) %% tau)

    # troughs
    bathy = Tacrophase - (tau / 2)

    peak_value   <- mesor + amplitude
    trough_value <- mesor - amplitude



    ### if day (24hrs) does not equal to tau
    if (!tau == day)  {
      Tacro2 <- ((acrophase *  tau / (2*pi)) %% day)
    names(Tacro2) <- paste0("Acrophase.time.",day)
    extra <- Tacro2

}

    if (factor == round(factor)) {
      f.list <-  seq.int(from = 1,
                         to   = factor,
                         by   = 1)

      f.list <- f.list - 1

      extra <- ((acrophase *  tau / (2*pi)) %% (tau * f.list))
      names(extra) <-  paste0("Acrophase.time.",(tau * f.list))
    }

  }





  ### For post-hoc and multicomponent cosinor-------------------

  ### Initial process
  y_h <- predict(model)
  M1  <- which.max(y_h)
  m1  <- which.min(y_h)

  acro.ph    <- M1 * Epc/(3600)
  bathy.ph   <- m1 * Epc/(3600)


  peak_value   <- y_h[M1]
  trough_value <- y_h[m1]
  mesor_vlaue  <- mean(c(trough_value, peak_value))

  Amp        <- (peak_value - trough_value)/2
  names(Amp) <- "Amplitude.post-hoc"

  post.hoc         <- c(mesor_vlaue, bathy.ph, trough_value, acro.ph, peak_value, Amp)
  names(post.hoc) <- c("MESOR.ph", "Bathyphase.ph.time", "Trough.ph", "Acrophase.ph.time", "Peak.ph", "Amplitude.ph")







  # Generate Output ---------------
  ## Inherit the output from lm
  if (dilute) {
    ### for bootstrap
    fit <- list(coef.cosinor = c(coef.cosinor, post.hoc))

    ## Assign Class
    class(fit) <- c("CosinorM")

  } else {

    fit <- model
    fit$model$time <- time
    fit$epoch <- Epc
    fit$tau <- tau
    # fit$time <- time
    fit$arctan2 <- arctan2
    fit$method <- method
    fit$type <- type
    fit$coef.cosinor <- coef.cosinor
    fit$extra <- extra
    fit$post.hoc <- post.hoc

    # Variance  ------------------
    ##  model parameters
    if (!type == "constant") {
      fit$vcov <- sandwich::vcovHC(model, type = type)
    } else {
      fit$vcov <- stats::vcov(model)
    }

    ## Coefficients
    VSEs <- se.CosinorM(fit)
    fit$se <- VSEs$se


    ## Assign Class
    class(fit) <- c("CosinorM","lm")
  }



  return(fit)

}
