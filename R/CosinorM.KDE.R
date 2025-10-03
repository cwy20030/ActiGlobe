#  File ActiGlobe/R/CosinorM.KDE.R
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
#' @title KDE-based circadian cosinor summary
#'
#' @description
#' Fit a wrapped Gaussian kernel density estimate (KDE) on circular time
#' (hours of day) weighted by activity and extract cosinor summaries:
#' MESOR, first-harmonic coefficients (β, γ), Amplitude and Acrophase.
#'
#' @details
#' This function builds a circular KDE from event times (hours in [0,24))
#' and non-negative weights (activity). The KDE is computed on a regular grid
#' over [0, 2*pi) using a wrapped Gaussian kernel with specified bandwidth.
#' Cosinor quantities are obtained by numerical integration of the KDE:
#' MESOR is the mean level, β and γ are the first-harmonic cosine and
#' sine coefficients, Amplitude is the magnitude of the first harmonic, and
#' Acrophase is the peak time expressed in radian and hour.
#'
#' @param time Numeric vector of event times in hours. Values are interpreted
#'   modulo 24 and must lie in [0, 24). If not numeric, the function will
#'   attempt to convert using an internal function: `C2T()`.
#' @param activity Numeric vector of non-negative weights corresponding to
#'   each time point. If not numeric, the function will coerce to numeric.
#' @param bw Numeric scalar. Kernel standard deviation on the circular scale
#'   in radian. Controls smoothing of the wrapped Gaussian kernel. Default 0.4.
#' @param grid Integer. Number of evaluation points for the circular KDE grid.
#'   Default 360 (i.e., one-degree resolution on the 0..2*pi circle).
#'
#' @details
#' The `"Wrapped Gaussian KDE"` method estimates rhythmic characteristics via a smooth circular density from weighted time-point observations, then extracts harmonic parameters via numerical integration over the circle. Unlike the closed-form solution of the traditional linear cosinor model [`CosinorM`], the model derives parameters from the fitted smoothed density.
#'
#' @section Circular KDE construction:
#' For each time coordinate, we can convert them into angular coordinates:
#' `θ = (time %% 24) / 24 * 2π`.
#' This procedure defines a regular grid over `[0, 2π]` for the KDE.
#'
#' To compute the KDE based on the recorded activities, the algorithm then compute the shortest distance between each time angular coordinate and each pre-defined grid point (default = 360 equally spaced points on the circle).
#' `diffs = (tg - θ + π) %% (2π) - π`
#' Note that points near the 0 or 2π cut are treated as neighbors rather than far apart.
#'
#' Given that the recorded activities closest to the a grid coordinate would contribute more localized information than those afar, the grid–time distances can then be used as weighting for each recording.
#' `f(gt) = sum(activity * kernel)`
#'
#' `kernel: A mean-centred Gaussian kernel.`
#' `f(gt): The estimated circular density at the angle gt`
#'
#' @section Harmonic integration:
#' Integrate the density and its projections using the trapezoid rule:
#'   `I0 = ∫ f(θ) dθ`
#'   `Icos = ∫ f(θ) * cos(θ) dθ`
#'   `Isin = ∫ f(θ) * sin(θ) dθ`
#'
#' These integrals approximate the first Fourier harmonic of the circular density.
#'
#' @section Parameter definitions:
#' `MESOR = I0 / (2π)`, the mean level of the density.
#' `a1 = Icos / π`, cosine coefficient.
#' `b1 = Isin / π`, sine coefficient.
#'
#' @section Amplitude estimation:
#' Amplitude (A) is calculated from the harmonic coefficients:
#' `A = sqrt(a1^2 + b1^2)`
#'
#' @section Acrophase interpretation:
#' Acrophase (φ) is derived from:
#' `φ = atan2(-b1, a1) %% (2π)`
#'
#' and converted to clock time:
#' `Acrophase_hour = φ * 24 / (2π)`
#'
#'
#' @return
#' A list of class c("CosinorM.KDE") with elements:
#'   * `method`: KDE
#'   * `tau`: 24 (period in hours)
#'   * `time`: The time coordiantes of the recording.
#'   * `kdf`: data.frame with columns
#'          - `θ`, angular grid in radians
#'          - `density`, KDE values
#'          - `hour` grid in hours
#'   * `coef.cosinor`: named numeric vector with entries `MESOR`, `Beta`, `Gamma`, `Amplitude`, `Acrophase.rad`, and `Acrophase.hr`
#'
#' @seealso `"cosinor"` model implementations and circular KDE methods
#'
#' @examples
#' require(stats)
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
#' fit <- CosinorM.KDE(time = df$Time,
#'                     activity = df$Activity,
#'                      bw = 0.4,
#'                      grid = 360)
#'
#' # inspect coefficients
#'
#' fit$coef.cosinor
#'
#' # plot KDE in hours
#' plot(fit$kdf$hour, fit$kdf$density, type = "l", xlab = "Hour", ylab = "KDE")
#'
#'
#' }
#' @keywords circular cosinor KDE circadian
#' @export


CosinorM.KDE <- function(time, activity, bw = 0.4, grid = 360) {
  if (!inherits(activity, "numeric")) activity <- as.numeric(as.character(activity))
  if (!inherits(time, "numeric")) time <- C2T(time)
  if (any(time < 0 | time >= 24)) {
    stop("time must be in [0,24). If you have midnight-to-midnight, ensure times are in that range.")
  }

  # Predefined function
  # helper: difference wrapped into (-pi,pi]
  wrap_diff <- function(a) {
    # map to -pi..pi
    x <- (a + pi) %% (2 * pi) - pi
    return(x)
  }

  # Trapezoid integration
  trap_int <- function(x, y) sum((y[-1] + y[-length(y)]) * diff(x)) / 2

  # convert hours -> radians in [0,2*pi)
  theta <- (time %% 24) / 24 * 2 * pi

  # wrapped gaussian KDE on grid
  grid_theta <- seq(0, 2 * pi, length.out = grid)

  # simple weighted kernel density on a linear grid with wrapping
  # wrapped gaussian kernel sum with weights
  dens <- sapply(grid_theta, function(tg) {
    diffs <- wrap_diff(tg - theta)
    sum(activity * dnorm(diffs, mean = 0, sd = bw))
  })

  kdf <- data.frame(theta = grid_theta, density = dens)
  kdf$hour <- kdf$theta * 24 / (2 * pi)

  # Numerical trapezoid integration on circular grid
  ord <- order(kdf$theta)
  theta_g <- kdf$theta[ord]
  f_g <- kdf$density[ord]

  # extend endpoint to ensure closed interval [0,2pi]
  theta_ext <- c(theta_g, theta_g[1] + 2 * pi)
  f_ext <- c(f_g, f_g[1])


  I0 <- trap_int(theta_ext, f_ext)                   # ∫ f
  Icos <- trap_int(theta_ext, f_ext * cos(theta_ext))# ∫ f cos
  Isin <- trap_int(theta_ext, f_ext * sin(theta_ext))# ∫ f sin





  MESOR <- I0 / (2 * pi)
  a1 <- Icos / pi
  b1 <- Isin / pi
  Amplitude <- sqrt(a1^2 + b1^2)
  Acrophase_rad <- (atan2(-b1, a1) %% (2 * pi))
  Acrophase_hour <- Acrophase_rad * 24 / (2 * pi)


  coef_cos <- c(
    MESOR = MESOR,
    Beta = a1,
    Gamma = b1,
    Amplitude = Amplitude,
    Acrophase.rad = Acrophase_rad,
    Acrophase.hr = Acrophase_hour
  )

  # Store Output
  fit <- list()

  fit$tau <- 24
  fit$time <- time
  fit$method <- "KDE"
  fit$kdf <- kdf
  fit$theta_g <- theta_g
  fit$f_g <- f_g
  fit$coef.cosinor <- coef_cos


  class(fit) <- c("CosinorM.KDE")
  return(fit)
}
