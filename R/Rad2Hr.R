#  File ActiGlobe/R/Rad2Hr.R
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
#' @title Rad2Hr
#' @description
#' Convert `acrophase` in radian to clock time based on tau.
#'
#' @param x Numeric vector of phases in radian.
#' @param tau Numeric scalar. Period length in the same time units you want returned (for hours use 24). Must be > 0 and <= 24.
#' @return Numeric vector of times in [0, tau). NA values propagate.
#' @examples
#' Rad2Hr(pi/2, tau = 24)
#'
#' Rad2Hr(c(-pi/2, 0, pi, 3*pi/2), tau = 24)
#'
#' @keywords acrophase radian
#' @export

Rad2Hr <- function(x, tau) {

  # coerce x to numeric while preserving NA
  if (!is.numeric(x)) x <- as.numeric(as.character(x))

  # validate tau
  if (!is.numeric(tau) || length(tau) != 1 || is.na(tau) || !is.finite(tau)) {
    stop("tau must be a single finite numeric value.")
  }
  if (!(tau > 0 && tau <= 24)) {
    stop("tau must be greater than 0 and less than or equal to 24.")
  }

  # conversion and wrap to [0, tau)
  acrophase_time <- (x * tau) / (2 * pi)
  out <- acrophase_time %% tau

  return(as.numeric(out))
}





