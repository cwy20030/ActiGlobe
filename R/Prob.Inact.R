#  File ActiGlobe/R/Prob.Inact.R
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
#' Fit a GLM to estimate inactive periods based on observed data
#' @import stats
#' @param y Numeric vector of observed activity counts
#' @param T Numeric vector of time indices corresponding to `y`
#' @param k Integer; degree of polynomial for time in the GLM. Default = 12
#' @param threshold Integer; number of times the predicted probability of inactivity
#'  must exceed 0.5 to classify the period as inactive. Default = 3
#' @param logical Logical scaler; TRUE will return logical vector. FALSE will return a summary table.
#' @returns
#' Logical vector; TRUE indicates inactive period
#' @noRd

Prob.Inact <- function(y, T, k = 12, threshold = 3, logical = TRUE) {
  # Parameters -------------------
  Epc <- min(diff(T), na.rm = TRUE)


  df <- data.frame(Act = y,
                   Time = T)
  df$y_log <- log(y)
  df$y_ina <- ifelse(is.infinite(df$y_log) & df$y_log < 0, 1, 0) #Inactive is 1

  fit_glm <- glm(y_ina ~ poly(Time, k), data = df, family = binomial)

  # predict on original times (discrete epochs)
  pred <- predict(fit_glm, newdata = data.frame(Time = T), type = "link", se.fit = TRUE)

  # transform from logit to probability and compute approximate 95% CI
  z <- qnorm(0.975)
  p_hat <- plogis(pred$fit)
  p_lo  <- plogis(pred$fit - z * pred$se.fit)
  p_hi  <- plogis(pred$fit + z * pred$se.fit)

  Inactive <- ifelse(cbind(p_lo,p_hat,p_hi) >0.5,1,0)
  inatv <- ifelse(rowSums(Inactive) >= threshold, TRUE, FALSE)


  if (logical) {
    return(inatv)

  } else {

    Out <-
    Table.Inact(inatv = inatv,
                T = T,
                Epc = Epc)

    Out <- na.omit(Out)
    return(Out)

  }


}



#' @title Table for Inactive Period
#' @noRd
Table.Inact <- function(inatv, T, Epc){



  ### Segment s ----------------
  x <- seq_len(length(inatv))

  x.inatv <- x[inatv]
  x1 <- c(1,diff(x.inatv))

  ### begining-------
  xm <- min(x.inatv, na.rm = TRUE)
  ini <- x.inatv[which(x1 > 1)]
  ini <- c(xm, ini)
  T.ini <- T[ini]

  ### end --------
  xM <- max(x.inatv, na.rm = TRUE)
  end <- x.inatv[which(x1 > 1) - 1]
  end <- c(end,xM)
  T.end <- T[end]

  ### duration
  duration <- (T.end - T.ini) + Epc


  Out <-  data.frame(start = T.ini,
                     duration = duration,
                     end = T.end)

  return(Out)

  }
