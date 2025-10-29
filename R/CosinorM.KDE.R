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
#' Fit a Gaussian kernel density estimate (KDE) on circular time
#' (hours of day) weighted by activity and extract cosinor summaries
#'
#' @details
#' This function builds a circularized KDE from event times (hours in [0,24))
#' and non-negative weights (activity). The KDE is computed on a regular grid
#' over [0, 2*pi) using a wrapped Gaussian kernel with specified bandwidth.
#' Cosinor parameters are obtained by numerical integration of the KDE:
#' MESOR is the mean level, \eqn{\beta} and \eqn{\gamma} are the first-harmonic
#' cosine and sine coefficients, Amplitude is the magnitude of the first harmonic,
#' and acrophase is the peak time expressed in radian and hour.
#'
#'  The KDE is computed for each grid angle \eqn{t_g} by summing weighted Gaussian
#'  densities of the wrapped angular differences \eqn{t_g - \theta_i}, where \eqn{\theta_i}
#'  are the input times converted to `radian`.
#'
#'  To mimic the cosinor-style phase mapping for clock time, it integrals KDE-
#'  predicted density \eqn{\hat{y}(\theta)} (or \eqn{f(\theta)}for normalization) and the complex-weighted
#'  density \eqn{e^{i\theta }\,\hat{y}(\theta)} (for circular mean/phase)
#'  over one cycle of a closed circular domain using the trapezoid rule. Note,
#'  the circularization procedure appends the first grid point to the last after
#'  shifting it by \eqn{2\pi} to close the circle.
#'
#'  Together, we can write the equation as,
#'  \deqn{I\theta=\int _0^{2\pi }f(\theta)\, d\theta}
#'  \deqn{C=\int _0^{2\pi }e^{i\theta}\, f(\theta)\, d\theta}
#'
#'  For \eqn{C}, we can then split into linearized cosinor equation:
#'  \deqn{C = Icos + iIsin}
#'
#' where,
#' \itemize{
#'   \item \eqn{Icos}: Integral of \eqn{f(\theta) * cos(\theta)} with respect to \eqn{(\theta)}.
#' \deqn{Icos = \int_{\theta}^{2\pi} f(\theta) \cos(\theta)\, d\theta}
#'
#'   \item \eqn{Isin}: Integral of \eqn{f(\theta) * sin(\theta)} with respect to \eqn{(\theta)}
#' \deqn{Isin = \int_{\theta}^{2\pi} f(\theta) \sin(\theta)\, d\theta}
#' }
#'
#'
#'  Trapezoid-rule discrete forms on grid \eqn{\theta_g} with values \eqn{f_g = f(\theta_g)}:
#'  \deqn{N\approx \sum_g\frac{f_{g+1}+f_g}{2}\, (\theta_{g+1} - \theta_g)}
#'  \deqn{C\approx \sum_g\frac{e^{i\theta_{g+1}}f_{g+1}+e^{i\theta_g}f_g}{2}\, (\theta_{g+1} - \theta_g)}
#'
#'
#'
#'
#' \strong{Conversion of cosinor integrals}:
#'
#' In a linearized single-phase cosinor model, the equation is often expressed as,
#'     \deqn{ \hat{y} = M + \beta cos(\frac{2\pi t} {\tau}) + \gamma sin(\frac{2\pi t} {\tau}) + \epsilon }
#' \itemize{
#'   \item \eqn{ M }: MESOR (mid-line estimating statistic of rhythm), the intercept
#'   \item \eqn{\beta = A * cos(\phi)}, estimated coefficient for the cosine term
#'   \item \eqn{\gamma = -A * sin(\phi)}, estimated coefficient for the sine term
#'   \item \eqn{ A }: Amplitude, peak deviation from M
#'   \item \eqn{ t }: Time coordinate within the cycle
#'   \item \eqn{ \tau }: The assumed period length
#'   \item \eqn{ \phi }: Acrophase (time-of-peak), computed from fitted sine and cosine coefficients
#'   \item \eqn{ \epsilon }: error term
#' }
#'
#' \strong{For KDE},
#' we can re-write these parameters since the KDE is projected onto an one cycle clock phase.
#' \itemize{
#'   \item \eqn{\theta}: the angular (circular) representation of clock time measured in radian.
#'   \item \eqn{ M }: MESOR (the mean of the KDE over the full cycle, i.e., 24 hours), \eqn{\frac{I\theta}{2\pi}}
#'   \item \eqn{\beta}: the coefficient of the cosine-weighted integral of the KDE, \eqn{\beta = \frac{Icos}{\pi}}
#'   \item \eqn{\gamma}: the coefficient of the sine-weighted integral of the KDE, \eqn{\gamma = \frac{Isin}{\pi}}
#'   \item \eqn{ A }: Amplitude, the peak activity at the first complex moment
#'   \deqn{C=\pi A\, e^{i\phi}\quad \Rightarrow \quad A=\frac{|C|}{\pi}}
#'   \item \eqn{\phi}: Acrophase (the mean circular phase), \eqn{ \phi = atan2(-\gamma, \beta) }
#' }
#'
#' Notes:
#' \itemize{
#'   \item The normalization factors \eqn{1/\pi} and \eqn{1/(2\pi)} follow from orthogonality of \eqn{\cos} and \eqn{\sin} on \eqn{[0,2\pi)} and from the usual least-squares/Fourier coefficient conventions.
#'
#'   \item While KDE does not assume the number of active-periods from the underlying rest-activity pattern, the algorithm can only extracts the first harmonic equivalent summary of the rest-activity pattern. This is because the time is set to be just one cycle.
#'
#' }
#'
#' @import stats
#' @param time Numeric vector of event times in hours. Values are interpreted
#'   modulo 24 and must lie in [0, 24). If not numeric, the function will
#'   attempt to convert using C2T.
#' @param activity Numeric vector of non-negative weights corresponding to
#'   each time point. If not numeric, the function will coerce to numeric.
#' @param bw Numeric scalar. Kernel standard deviation on the circular scale
#'   (radians). Controls smoothing of the wrapped Gaussian kernel. Default 0.4.
#' @param grid Integer. Number of evaluation points for the circular KDE grid.
#'   Default 360 (i.e., one-degree resolution on the \eqn{ 0.2\pi } circle).
#'
#' @return A list of class c("CosinorM.KDE") with elements:
#' \itemize{
#'   \item tau: Period in hours
#'   \item kdf: A data.frame with the following columns:
#'     \itemize{
#'       \item theta: Angular grid in radians
#'       \item density: KDE values
#'       \item hour: Grid in hours
#'     }
#'   \item coef.cosinor: Named numeric vector with entries:
#'     \itemize{
#'       \item MESOR: the mean of activity density over 24 hours
#'       \item Amplitude: the peak activity at the most active period of the day.
#'       \item Acrophase: Acrophase in radian (time-of-peak) of the most active period during the day.
#'       \item Acrophase.hr: the timing of the peak activity at the most active period of the day.
#'       \item Beta: the coefficient of the cosine-weighted integral of the KDE,
#'       \item Gamma: the coefficient of the sine-weighted integral of the KDE,
#'     }
#' }
#'
#' @seealso CosinorM
#'
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
#'
#' fit <- CosinorM.KDE(time = df$Time,
#'            activity = df$Activity)
#'
#' # inspect coefficients
#' fit$coef.cosinor
#'
#' # plot KDE in hours
#' plot(fit$kdf$hour, fit$kdf$density, type = "l", xlab = "Hour", ylab = "KDE")
#'
#' }
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
    # map to -pi and pi
    x <- (a + pi) %% (2 * pi) - pi
    return(x)
  }

  trap_int <- function(x, y) sum((y[-1] + y[-length(y)]) * diff(x)) / 2

  # convert hours -> radians in [0,2*pi)
  theta <- (time %% 24) / 24 * 2 * pi

  # wrapped gaussian KDE on grid
  grid_theta <- seq(0, 2 * pi, length.out = grid)

  # simple weighted kernel density on a linear grid with wrapping
  # wrapped Gaussian kernel sum with weights
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


  I0 <- trap_int(theta_ext, f_ext)                   # integral f
  Icos <- trap_int(theta_ext, f_ext * cos(theta_ext))# integral f cos
  Isin <- trap_int(theta_ext, f_ext * sin(theta_ext))# integral f sin

  ## Extract MESOR, beta, gamma
  mesor <- I0 / (2 * pi)
  beta <- Icos / pi
  gamma <- Isin / pi

  ## Compute amplitude and acrophase
  amplitude <- sqrt(beta^2 + gamma^2)

  acrophase <- theta <- atan(abs(gamma) / beta)

  Bs <- beta
  Gs <- gamma
  acrophase <- ifelse(Bs >= 0 & Gs > 0, -theta,
                            ifelse(Bs < 0 & Gs >= 0, theta - pi,
                                   ifelse(Bs <= 0 & Gs < 0, -theta - pi,
                                          ifelse(Bs > 0 & Gs <= 0, theta - (2 * pi), NA))))


  Acrophase_hour <- acrophase * 24 / (2 * pi)


  coef_cos <- c(
    MESOR = mesor,
    Amplitude = amplitude,
    Acrophase = acrophase,
    Acrophase.hr = Acrophase_hour,
    Beta = beta,
    Gamma = gamma
  )

  # Store Output
  fit <- list()

  fit$tau <- 24
  fit$kdf <- kdf
  fit$coef.cosinor <- coef_cos


  class(fit) <- c("CosinorM.KDE")
  return(fit)
}
