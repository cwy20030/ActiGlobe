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
#' KDE-based circadian cosinor summary
#'
#' Fit a wrapped-Gaussian kernel density estimate (KDE) on circular time
#' (hours of day) weighted by activity and extract cosinor summaries:
#' MESOR, first-harmonic coefficients (Beta, Gamma), Amplitude and Acrophase.
#'
#' This function builds a circular KDE from event times (hours in [0,24))
#' and nonnegative weights (activity). The KDE is computed on a regular grid
#' over [0, 2*pi) using a wrapped Gaussian kernel with specified bandwidth.
#' Cosinor quantities are obtained by numerical integration of the KDE:
#' MESOR is the mean level, Beta and Gamma are the first-harmonic cosine and
#' sine coefficients, Amplitude is the magnitude of the first harmonic, and
#' Acrophase is the peak time expressed in radian and hour.
#'
#' @param time Numeric vector of event times in hours. Values are interpreted
#'   modulo 24 and must lie in [0, 24). If not numeric, the function will
#'   attempt to convert using C2T.
#' @param activity Numeric vector of non-negative weights corresponding to
#'   each time point. If not numeric, the function will coerce to numeric.
#' @param bw Numeric scalar. Kernel standard deviation on the circular scale
#'   (radians). Controls smoothing of the wrapped Gaussian kernel. Default 0.4.
#' @param grid Integer. Number of evaluation points for the circular KDE grid.
#'   Default 360 (i.e., one-degree resolution on the 0..2*pi circle).
#'
#' @details
#' - The KDE is computed for each grid angle t_g by summing weighted Gaussian
#'   densities of the wrapped angular differences t_g - theta_i, where theta_i
#'   are the input times converted to `radian`.
#' - Numerical integration over the closed circular interval is performed with
#'   the trapezoid rule after ordering the grid and appending the first grid
#'   point shifted by 2*pi to close the circle.
#' - Cosinor integrals used:
#'   - I0 = ∫ f(theta) d-theta
#'   - Icos = ∫ f(theta) cos(theta) d-theta
#'   - Isin = ∫ f(theta) sin(theta) dtheta
#'   - MESOR = I0 / (2*pi)
#'   - Beta = a1 = Icos / pi
#'   - Gamma = b1 = Isin / pi
#'   - Amplitude = sqrt(a1^2 + b1^2)
#'   - Acrophase (radians) = atan2(-b1, a1) modulo 2*pi
#'   - Acrophase (hours) = Acrophase.rad * 24 / (2*pi)
#'
#' @return A list of class c("CosinorM.KDE") with elements:
#'   - tau: 24 (period in hours)
#'   - kdf: data.frame with columns
#'       **theta** angular grid in radians; **density** KDE values; **hour** grid in hours
#'   - coef.cosinor: named numeric vector with entries
#'       **MESOR**, **Beta**, **Gamma**, **Amplitude**, **Acrophase.rad**, **Acrophase.hr**
#'
#' @seealso cosinor model implementations and circular KDE methods
#'
#' @examples
#' # simulate times (hours) and activity weights
#' set.seed(1)
#' times <- runif(200, 0, 24)
#' activity <- pmax(0, rnorm(200, mean = 1 + 0.5 * cos(2 * pi * times / 24 - 1), sd = 0.5))
#' fit <- CosinorM.KDE(times, activity)
#' # inspect coefficients
#' fit$coef.cosinor
#' # plot KDE in hours
#' plot(fit$kdf$hour, fit$kdf$density, type = "l", xlab = "Hour", ylab = "KDE")
#'
#'
#'
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
  fit$kdf <- kdf
  fit$coef.cosinor <- coef_cos


  class(fit) <- c("CosinorM.KDE")
  return(fit)
}
