#  File ActiGlobe/R/CosinorM.KDE.R
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
#' @title KDE-based circadian cosinor summary
#'
#' @description
#' Fit a Gaussian kernel density estimate (KDE) on circular time (hours of day)
#' weighted by activity and extract first-harmonic cosinor summaries.
#'
#' @details
#' This function builds a circular KDE from event times (hours in \[0,24)) and
#' activity. The KDE is computed on a regular angular grid over \[0, 2*pi) using
#' a wrapped Gaussian kernel with specified bandwidth. Cosinor summaries (MESOR,
#' Beta, Gamma, Amplitude, Acrophase) are obtained by numerical integration of
#' the KDE using trapezoid-rule quadrature.
#'
#' Cosinor parameters are obtained by numerical integration of the KDE on the
#' grid:
#' \itemize{
#'   \item MESOR: mean level of the fitted curve, \eqn{M = I_0 / (2\pi)}.
#'   \item \eqn{\beta}: cosine coefficient, \eqn{\beta = I_{cos} / \pi}.
#'   \item \eqn{\gamma}: sine coefficient, \eqn{\gamma = I_{sin} / \pi}.
#'   \item Amplitude: magnitude of the first harmonic, \eqn{A = \sqrt{\beta^2 +
#'   \gamma^2}}.
#'   \item Acrophase: peak time, \eqn{\phi = atan2(-\gamma, \beta)}, expressed
#'   in radians and converted to hours.
#' }
#'
#' The grid integrals are computed with trapezoid weights \eqn{w_g}:
#' \deqn{I_0 = \text{area}_g \sum_g \frac{pdf_g \, w_g}{den_g}}
#' \deqn{I_{cos} = \text{area}_g \sum_g \frac{pdf_g \cos(\theta_g)\,
#' w_g}{den_g}}
#' \deqn{I_{sin} = \text{area}_g \sum_g \frac{pdf_g \sin(\theta_g)\,
#' w_g}{den_g}}
#'
#' where \eqn{pdf_g} is the normalized KDE at grid angle \eqn{\theta_g},
#' \eqn{den_g}
#' is the kernel denominator at that grid point, and \eqn{\text{area}_g}
#' rescales the fitted density back to the activity scale.
#'
#' Notes:
#' \itemize{
#'   \item Normalization factors \eqn{1/\pi} and \eqn{1/(2\pi)} follow from the
#'   orthogonality of sine and cosine on \eqn{[0,2\pi)}.
#'   \item The KDE summarizes the first harmonic of the rest–activity pattern
#'   over one cycle; higher harmonics are not extracted.
#'   \item The trapezoid rule ensures numerical stability by integrating over
#'   a dense, evenly spaced grid.
#'   \item The dense grid excludes the duplicate \eqn{2\pi} endpoint (grid in
#'   \eqn{[0,2\pi)}) to avoid duplicated quadrature nodes.
#'   \item Grid and observation trapezoid weights correct for irregular
#'   sampling and yield stable approximations to continuous integrals.
#'   \item Grid points with near-zero denominator \eqn{\sum_i K(\theta-\theta_i)
#'    w_i} are set to \code{NA} and excluded from integrals to avoid
#'    division-by-zero artifacts.
#' }
#'
#' \strong{Residaul Variance and Effective Degree of Freedom}
#' \itemize{
#'   \item \strong{Regression (projection hat matrix):}
#'
#'   \deqn{\hat{\sigma}^2 = \frac{\mathrm{RSS}}{n - p},}
#'
#'   where \eqn{p} is the number of parameters (degrees of freedom used by the
#'   fit).
#'
#'   \item \strong{Linear kernel smoothing:}
#'
#'   Fitted values are given by the linear smoother
#'
#'   \deqn{\hat{y} = W y,}
#'
#'   where \eqn{W} is the hat matrix induced by the kernel weights (not a
#'   projection).
#' }
#'
#' When defining residuals as \eqn{r = y - \hat{y}} and \eqn{\mathrm{RSS} =
#' \sum r_i^2},
#' the unbiased estimator under the Gaussian kernel smoother can be written as
#' \deqn{\hat{\sigma}^2 = \frac{\mathrm{RSS}}{\,n - 2\,\mathrm{tr}(W) +
#' \mathrm{tr}(W^{\top} W)\,}.}
#'
#' where,
#' \itemize{
#'   \item \deqn{\mathrm{tr}(W)} is the effective degrees of freedom used by
#'   the smoother
#'         (analogous to the number of parameters).
#'   \item \deqn{\mathrm{tr}(W^{\top} W) = \sum_{i,j} W_{ij}^2} is a correction
#'    term because
#'         \eqn{W} is not an orthogonal projection.
#' }
#'
#' @param time Numeric vector of time coordinates for each data point. Values
#'  are interpreted modulo 24 and must lie in \eqn{[0, 24)}. If not numeric,
#'  an internal function will be used to convert it to numeric values in unit
#'  of hour.
#' @param activity Numeric vector of activity counts from an actigraphy device
#' in correspondance to each time point. Non-numeric inputs will be coerced.
#' All-zero or non-finite activity values produce an error.
#' @param bw Numeric scalar. Kernel standard deviation (SD) controlling s
#' moothing. bw is interpreted in the same units as the angular scale after
#' conversion: the implementation converts `bw` from hours to radians
#' internally using \eqn{bw_{rad} = bw * 2\pi / \tau}. Default: 0.8.
#' Note, small \code{bw} (approaching 0) can cause numerical instability, while
#' arge \code{bw} (approaching 1.5) leads a near-uniform kernel and reduces
#' temporal resolution.
#' @param grid Integer number of evaluation points used for the dense grid on
#'   \eqn{[0, 2 * \pi)}. The grid excludes the duplicate 2*pi endpoint.
#'   Default: 360.
#' @param arctan2 Logical; if TRUE (default) acrophase is computed with
#'   \code{atan2(gamma, beta)}, resulting in the quadrant interval between
#'   \eqn{-\pi} and \eqn{\pi}. Whereas, when set to FALSE, the legacy arctangent
#'   quadrant is mapped. The resulting interval lies between
#'   \eqn{-\frac{\pi}{2}} and \eqn{\frac{\pi}{2}}.
#' @param dilute Logical; if FALSE (default), all essential parameters would be
#' produced. When set to TRUE, only cosinor coefficients are returned. This is
#' suited for post-hoc processes, such as computing confidence interval via
#' nonparametric bootstrap
#'
#' @return A list of class \code{c("CosinorM.KDE")} with elements:
#' \itemize{
#'   \item parm: Parameters specified in the model (list with \code{time},
#'   \code{activity}, \code{bw}, \code{grid})
#'   \item tau: Period in hours (default to 24hour)
#'   \item kdf: A data.frame with the following columns:
#'     \itemize{
#'       \item theta: Angular positions (radians) at the observation angles
#'       \item density: Estimated PDF values at the observation angles
#'       (integrates to 1 over \eqn{[0,2\pi)} when scaled)
#'       \item trapizoid.weight: Numeric vector of trapezoid integration
#'       weights at observation angles
#'       \item kernel.weight: Denominator from kernel convolution (kernel mass
#'       at each observation angle)
#'       \item fitted.values: Normalized fitted KDE (activity-scale) at
#'       observation angles
#'       \item fitted.var: Variance estimate of the fitted values (for SE)
#'       \item fitted.se: Standard error of the fitted values
#'       \item hour: Corresponding clock time in hours
#'     }
#'   \item coef.cosinor: Named numeric vector with entries:
#'     \itemize{
#'       \item MESOR: the mean of activity density over 24 hours
#'       \item Amplitude: the first-harmonic amplitude derived from Beta and
#'       Gamma
#'       \item Acrophase: Acrophase in radians (time-of-peak) of the dominant
#'       daily component
#'       \item Acrophase.hr: Acrophase converted to clock hours in [0,24)
#'       \item Beta: coefficient equivalent to the cosine-weighted integral of
#'       the KDE
#'       \item Gamma: coefficient equivalent to the sine-weighted integral of
#'       the KDE
#'     }
#'   \item post.hoc: Post-hoc peak/trough diagnostics derived from
#'   fitted.values at observation angles (MESOR.ph, Bathyphase.ph.time,
#'   Trough.ph, Acrophase.ph.time, Peak.ph, Amplitude.ph)
#'   \item grid: (when \code{dilute = FALSE}) list of grid diagnostics:
#'     \itemize{
#'       \item theta: evaluation angles on the dense grid (radians)
#'       \item density: Grid-level PDF (integrates to 1 over theta)
#'       \item trapizoid.weight: Trapezoid integration weights for the grid
#'       points
#'       \item kernel.weight: Kernel mass (denominator) at each grid point
#'       \item fitted.values: Normalized fitted values at grid points
#'       (activity-scale)
#'       \item fitted.var: Variance propagation through fitted variances on
#'       the grid
#'       \item fitted.se: Standard error of the fitted values at grid points
#'     }
#' }
#'
#'
#' @section Warnings and edge cases:
#'  \itemize{
#'       \item The function errors if \code{all(activity) == 0} or if
#'       \code{activity} contains
#'   non-finite values.
#'       \item Very small \code{bw} can cause near-zero denominator values
#'       (numerical instability) and produce NA or an error; very large
#'       \code{bw} approaches a near-uniform kernel and will wash out temporal
#'       structure.
#' }
#'
#' @references
#' Cornelissen G. Cosinor-based rhythmometry. Theoretical Biology and Medical
#' Modelling. 2014-12-01 2014;11(1):16. doi:10.1186/1742-4682-11-16
#'
#' 	On Nonparametric Density Estimation for Circular Data: An Overview ,
#' 	bookTitle= Directional Statistics for Innovative Applications: A
#' 	Bicentennial Tribute to Florence Nightingale. Springer Nature Singapore;
#' 	2022:351--378.
#'
#' Cremers J, Klugkist I. One Direction? A Tutorial for Circular Data Analysis
#' Using R With Examples in Cognitive Psychology. Methods. Front Psychol.
#' 2018-October-30 2018;9(2040):2040. doi:10.3389/fpsyg.2018.02040
#'
#' @seealso \code{\link{CosinorM}}  \code{\link{ggCosinorM}}
#'
#' @examples
#' \dontrun{
#' # Import data
#' FlyEast
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
#' df <- subset (
#'     x = df,
#'     subset = df$Date == "2017-10-27"
#' )
#'
#'
#' fit <- CosinorM.KDE (
#'     time = df$Time,
#'     activity = df$Activity
#' )
#'
#' # inspect coefficients
#' fit$coef.cosinor
#'
#' # plot KDE in hours
#' plot (
#'     x = fit$kdf$hour,
#'     y = fit$kdf$density,
#'     type = "l",
#'     xlab = "Hour",
#'     ylab = "KDE"
#' )
#' }
#'
#' @keywords circular cosinor KDE circadian
#' @export

CosinorM.KDE <- function (time, activity, bw = 0.8, grid = 360L,
                          arctan2 = TRUE, dilute = FALSE) {
    # Check Point and Input Validation -------------------------
    activity <- ValInput (x = activity, type = "Act")
    time <- ValInput (x = time, type = "Tm")
    if (!is.numeric (bw) || length (bw) != 1 || bw <= 0) {
        stop ("bw must be a positive numeric scalar")
    }
    if (!is.integer (grid)) grid <- as.integer (grid)

    ## Get Essential Info -----------------
    tau <- 24
    dt <- diff (time)
    dt <- dt [dt > 0]
    Epc <- 1 / min (dt)

    ## Convert to radians and compute observation-level KDE
    obs_results <- compute_obs_kde (time, activity, tau, bw)
    theta <- obs_results$theta
    y_h <- obs_results$y_h
    Pdf <- obs_results$Pdf
    w <- obs_results$w
    K <- obs_results$K
    den <- obs_results$den
    n <- obs_results$n

    ## Compute variance for fitted curve
    variance_results <- compute_variance (activity, y_h, K, w, den, n)
    resid <- variance_results$resid
    df_denom <- variance_results$df_denom
    sigma_hat <- variance_results$sigma_hat
    var_fitted <- variance_results$var_fitted
    se_fitted <- variance_results$se_fitted


    ## Evaluate on grid and compute cosinor parameters
    grid_results <- compute_grid_kde (theta, activity, w, bw, tau,
                                      grid, sigma_hat)
    gtheta <- grid_results$gtheta
    y.g <- grid_results$y.g
    pdf.g <- grid_results$pdf.g
    w.g <- grid_results$w.g
    den.g <- grid_results$den.g
    var_fitted.g <- grid_results$var_fitted.g
    se_fitted.g <- grid_results$se_fitted.g
    area.g <- grid_results$area.g

    ## Compute cosinor parameters
    cosinor_params <- compute_cosinor_params (pdf.g, gtheta, w.g, den.g,
                                              area.g, arctan2)
    mesor <- cosinor_params$mesor
    beta <- cosinor_params$beta
    gamma <- cosinor_params$gamma
    amplitude <- cosinor_params$amplitude
    acrophase <- cosinor_params$acrophase

    ## Prepare output coefficients
    coef.cosinor <- prepare_coef_output (mesor, amplitude, acrophase,
                                         beta, gamma)
    post.hoc <- compute_posthoc (y_h, Epc)

    ## Generate final output structure
    fit <- build_kde_output (dilute, coef.cosinor, post.hoc, activity, time,
                            tau, bw, grid, arctan2, resid, df_denom, theta,
                            Pdf, w, den, y_h, var_fitted, se_fitted, gtheta,
                            pdf.g, w.g, den.g, y.g, var_fitted.g, se_fitted.g)

    class (fit) <- c ("CosinorM.KDE")
    return (fit)
}


## Helper functions for CosinorM.KDE ---------------

#' @title Compute Observation-Level KDE
#' @noRd
compute_obs_kde <- function (time, activity, tau, bw) {
    theta <- (time %% tau) * 2 * pi / tau
    n <- length (activity)
    w <- trap_weights (theta = theta)

    diffs_mat <- wrap_diff (outer (theta, theta, "-"))
    bw_rad <- bw * 2 * pi / tau
    K <- dnorm (diffs_mat, mean = 0, sd = bw_rad)

    den <- as.numeric ((K %*% w))
    dens <- as.numeric (K %*% (activity * w))
    y_h <- dens / den

    area <- trap_int (theta, y_h)
    if (!is.finite (area) || area <= .Machine$double.eps) {
        stop ("area is zero or invalid; check activity and bw")
    }

    Pdf <- dens / area

    list (theta = theta, y_h = y_h, Pdf = Pdf, w = w, K = K, den = den, n = n)
}


#' @title Compute Variance for Fitted Curve
#' @noRd
compute_variance <- function (activity, y_h, K, w, den, n) {
    W <- K * matrix (rep (w, each = nrow (K)), nrow = nrow (K),
                     byrow = FALSE)
    W <- W / matrix (rep (den, times = ncol (W)), nrow = nrow (W),
                     byrow = FALSE)

    resid <- activity - y_h
    RSS <- sum (resid^2)

    trW <- sum (diag (W))
    trWW <- sum (W * W)
    df_denom <- n - (2 * trW) + trWW
    if (df_denom <= 0) {
        stop ("Nonpositive denominator for sigma^2 estimate;
              increase bw or check W")
    }

    sigma_hat <- RSS / df_denom
    var_fitted <- sigma_hat * rowSums (W^2)
    se_fitted <- sqrt (var_fitted)

    list (resid = resid, df_denom = df_denom, sigma_hat = sigma_hat,
          var_fitted = var_fitted, se_fitted = se_fitted)
}


#' @title Compute Grid-Level KDE
#' @noRd
compute_grid_kde <- function (theta, activity, w, bw, tau, grid, sigma_hat) {
    bw_rad <- bw * 2 * pi / tau
    gtheta <- seq (0, 2 * pi, length.out = as.integer (grid) + 1)
    gtheta <- gtheta [-(length (gtheta))]

    diffs.g <- wrap_diff (outer (gtheta, theta, "-"))
    K.g <- dnorm (diffs.g, mean = 0, sd = bw_rad)

    w.g <- trap_weights (theta = gtheta)
    dens.g.num <- as.numeric (K.g %*% (activity * w))
    den.g <- as.numeric (K.g %*% w)

    eps <- .Machine$double.eps
    zero_mask <- den.g <= eps
    y.g <- compute_grid_y (dens.g.num, den.g, zero_mask)

    area.g <- trap_int (gtheta, y.g)
    if (!is.finite (area.g) || area.g <= .Machine$double.eps) {
        stop ("area.g is zero or invalid; check activity and bw / grid")
    }

    pdf.g <- dens.g.num / area.g

    W.g <- K.g * matrix (rep (w, each = nrow (K.g)), nrow = nrow (K.g))
    W.g <- W.g / matrix (rep (den.g, times = ncol (W.g)),
                        nrow = nrow (W.g), byrow = FALSE)

    var_fitted.g <- sigma_hat * rowSums (W.g^2)
    se_fitted.g <- sqrt (var_fitted.g)

    list (gtheta = gtheta, y.g = y.g, pdf.g = pdf.g, w.g = w.g, den.g = den.g,
          var_fitted.g = var_fitted.g, se_fitted.g = se_fitted.g,
          area.g = area.g)
}


#' @title Compute Grid Y Values
#' @noRd
compute_grid_y <- function (dens.g.num, den.g, zero_mask) {
    if (any (zero_mask)) {
        y.g <- rep (NA_real_, length (den.g))
        y.g [!zero_mask] <- dens.g.num [!zero_mask] / den.g [!zero_mask]
    } else {
        y.g <- dens.g.num / den.g
    }
    y.g
}


#' @title Compute Cosinor Parameters
#' @noRd
compute_cosinor_params <- function (pdf.g, gtheta, w.g, den.g, area.g,
                                    arctan2) {
    I0_act <- area.g * sum ((pdf.g * w.g) / den.g, na.rm = TRUE)
    Icos_act <- area.g * sum ((pdf.g * cos (gtheta) * w.g) / den.g,
                              na.rm = TRUE)
    Isin_act <- area.g * sum ((pdf.g * sin (gtheta) * w.g) / den.g,
                              na.rm = TRUE)

    mesor <- I0_act / (2 * pi)
    beta <- Icos_act / pi
    gamma <- Isin_act / pi
    amplitude <- sqrt (beta^2 + gamma^2)

    acrophase <- compute_acrophase (beta, gamma, arctan2)

    list (mesor = mesor, beta = beta, gamma = gamma, amplitude = amplitude,
          acrophase = acrophase)
}


#' @title Compute Acrophase
#' @noRd
compute_acrophase <- function (beta, gamma, arctan2) {
    if (arctan2) {
        atan2 (gamma, beta)
    } else {
        theta <- atan (abs (gamma) / beta)
        ifelse (beta >= 0 & gamma > 0, -theta,
            ifelse (beta < 0 & gamma >= 0, theta - pi,
                ifelse (beta <= 0 & gamma < 0, -theta - pi,
                    ifelse (beta > 0 & gamma <= 0, theta - (2 * pi), NA)
                )
            )
        )
    }
}


#' @title Prepare Coefficient Output
#' @noRd
prepare_coef_output <- function (mesor, amplitude, acrophase, beta, gamma) {
    coef_names <- c ("MESOR", "Amplitude.24", "Acrophase.24",
                    "Beta.24", "Gamma.24")
    coef.cosinor <- unname (c (mesor, amplitude, acrophase, beta, gamma))
    names (coef.cosinor) <- coef_names
    coef.cosinor
}


#' @title Compute Post-Hoc Parameters
#' @noRd
compute_posthoc <- function (y_h, Epc) {
    M1 <- which.max (y_h)
    m1 <- which.min (y_h)

    acro.ph <- M1 * Epc / (3600)
    bathy.ph <- m1 * Epc / (3600)

    peak_value <- y_h [M1]
    trough_value <- y_h [m1]
    mesor_vlaue <- mean (c (trough_value, peak_value))

    Amp <- (peak_value - trough_value) / 2

    post.hoc <- c (mesor_vlaue, bathy.ph, trough_value,
                   acro.ph, peak_value, Amp)
    names (post.hoc) <- c ("MESOR.ph", "Bathyphase.ph.time", "Trough.ph",
                          "Acrophase.ph.time", "Peak.ph", "Amplitude.ph")
    post.hoc
}


#' @title Build KDE Output Structure
#' @noRd
build_kde_output <- function (dilute, coef.cosinor, post.hoc, activity, time,
                              tau, bw, grid, arctan2, resid, df_denom, theta,
                              Pdf, w, den, y_h, var_fitted, se_fitted, gtheta,
                              pdf.g, w.g, den.g, y.g, var_fitted.g,
                              se_fitted.g) {
    if (dilute) {
        list (coef.cosinor = c (coef.cosinor, post.hoc))
    } else {
        kdf <- data.frame (
            theta = theta,
            density = Pdf,
            trapizoid.weight = w,
            kernel.weight = den,
            fitted.values = y_h,
            fitted.var = var_fitted,
            fitted.se = se_fitted
        )

        list (
            model = data.frame (activity = activity, time = time),
            tau = tau,
            bw = bw,
            grid.size = grid,
            arctan2 = arctan2,
            dilute = dilute,
            residuals = resid,
            edf.residual = df_denom,
            kdf = kdf,
            coef.cosinor = coef.cosinor,
            post.hoc = post.hoc,
            grid = list (
                theta = gtheta,
                density = pdf.g,
                trapizoid.weight = w.g,
                kernel.weight = den.g,
                fitted.values = y.g,
                fitted.var = var_fitted.g,
                fitted.se = se_fitted.g
            )
        )
    }
}


# Pre-defined helper functions -------------------------------
#' @title mapping angular differences into the principal interval (-pi, pi]
#' @noRd
wrap_diff <- function (a) {
    (a + pi) %% (2 * pi) - pi
}

#' @title trapizoid integeral
#' @noRd
trap_int <- function (x, y) sum ((y [-1] + y [-length (y)]) * diff (x)) / 2

#' @title trapizoid weight
#' @noRd
trap_weights <- function (theta, ext = FALSE) {
    n <- length (theta)
    if (n == 1) {
        return (1)
    }
    dx <- diff (theta)
    if (isTRUE (ext)) {
        n2 <- n - 1
        w <- numeric (n2)
        w [1] <- (dx [1] + dx [n2]) / 2
        if (n2 > 2) w [2:(n2 - 1)] <- (dx [1:(n2 - 2)] + dx [2:(n2 - 1)]) / 2
        w [n2] <- (dx [n2 - 1] + dx [n2]) / 2
    } else {
        w <- numeric (n)
        w [1] <- dx [1] / 2
        w [n] <- dx [n - 1] / 2
        if (n > 2) w [2:(n - 1)] <- (dx [-length (dx)] + dx [-1]) / 2
    }
    w
}
