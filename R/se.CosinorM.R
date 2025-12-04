#  File ActiGlobe/R/se.CosinorM.R
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
#' @title standard error for `Cosinor` coefficients
#'
#' @description
#' Delta‑method derivatives and variances for cosinor amplitude and acrophase
#'
#'
#' @details
#'
#' Compute partial derivatives, and delta‑method variances for amplitude and
#' acrophase from linearized cosinor coefficients for cosine (\eqn{\beta}) and
#' sine (\eqn{\gamma}) waves, using the variance–covariance matrix. Note that
#' the matrix is k-by-k, where \eqn{k = 2 * numbers of \tau specified}. See \code{\link{CosinorM}}
#' The cosinor mapping is:
#'
#' \strong{Amplitude} (point estimate, \eqn{A = \sqrt{\beta^2 + \gamma^2}}):
#'
#' Partial derivatives (gradient) are:
#' \eqn{\frac{\partial A}{\partial \beta} = \frac{\beta}{A}}
#' \eqn{\frac{\partial A}{\partial \gamma} = \frac{\gamma}{A}}
#'
#' Delta-method variance formulas (quadratic form) are:
#' \deqn{\operatorname{Var}(A) \approx \left(\frac{\partial A}{\partial \beta}\right)^2 \operatorname{Var}(\beta)
#' \;+\; 2\left(\frac{\partial A}{\partial \beta}\right)\left(\frac{\partial A}{\partial \gamma}\right)\operatorname{Cov}(\beta,\gamma)
#' \;+\; \left(\frac{\partial A}{\partial \gamma}\right)^2 \operatorname{Var}(\gamma).}
#'
#' \strong{Acrophase} (point estimate, \eqn{\phi = \operatorname{atan2}(\gamma,\beta)}):
#' Equivalently,
#' \eqn{\tan(\phi)=\frac{\gamma}{\beta}.}
#'
#' Differentiate both sides treating \eqn{\phi} as a function of \eqn{\beta} and \eqn{\gamma}.
#'
#' With respect to \eqn{\beta}:
#' \eqn{\frac{d}{d\beta}\tan(\phi)=\frac{d}{d\beta}\left(\frac{\gamma}{\beta}\right).}
#' By the chain rule on the left:
#' \eqn{\sec^2(\phi)\,\frac{\partial\phi}{\partial\beta} = -\frac{\gamma}{\beta^2}.}
#' Solve for \eqn{\partial\phi/\partial\beta}:
#' \eqn{\frac{\partial\phi}{\partial\beta} = -\frac{\gamma}{\beta^2 \sec^2(\phi)}.}
#' Use \eqn{\sec^2(\phi)=1+\tan^2(\phi)=1+\left(\frac{\gamma}{\beta}\right)^2=\frac{\beta^2+\gamma^2}{\beta^2}}
#' to obtain:
#' \eqn{\frac{\partial\phi}{\partial\beta} = -\frac{\gamma}{\beta^2+\gamma^2}.}
#'
#' With respect to \eqn{\gamma}:
#' \eqn{\frac{d}{d\gamma}\tan(\phi)=\frac{d}{d\gamma}\left(\frac{\gamma}{\beta}\right).}
#' By the chain rule on the left:
#' \eqn{\sec^2(\phi)\,\frac{\partial\phi}{\partial\gamma} = \frac{1}{\beta}.}
#' Solve for \eqn{\partial\phi/\partial\gamma}:
#' \eqn{\frac{\partial\phi}{\partial\gamma} = \frac{1}{\beta \sec^2(\phi)} = \frac{\beta}{\beta^2+\gamma^2}.}
#'
#' Partial derivatives (compact form) are:
#' \eqn{\frac{\partial \phi}{\partial \beta} = -\frac{\gamma}{\beta^2 + \gamma^2}}
#' \eqn{\frac{\partial \phi}{\partial \gamma} =  \frac{\beta}{\beta^2 + \gamma^2}}
#'
#' Delta-method variance formulas (quadratic form) are:
#' \eqn{\operatorname{Var}(\phi) \approx \left(\frac{\partial \phi}{\partial \beta}\right)^2 \operatorname{Var}(\beta)
#' \;+\; 2\left(\frac{\partial \phi}{\partial \beta}\right)\left(\frac{\partial \phi}{\partial \gamma}\right)\operatorname{Cov}(\beta,\gamma)
#' \;+\; \left(\frac{\partial \phi}{\partial \gamma}\right)^2 \operatorname{Var}(\gamma).}
#'
#'
#' @import stats
#'
#'
#' @param object A model of class `CosinorM`
#' @param method Only supports delta for now
#' Standard errors are the square roots of the approximated variances.
#'
#'
#' @return A named list with components:
#' \itemize{
#'   \item \code{var.Amplitude}: Approximated variance of amplitude.
#'   \item \code{var.Acrophase}: Approximated variance of acrophase.
#'   \item \code{se.Amplitude}: Delta‑method standard error for amplitude.
#'   \item \code{se.Acrophase}: Delta‑method standard error for acrophase.
#' }
#'
#'
#' @seealso \code{\link{vcov}}
#'
#'
#' @examples
#' \dontrun{
#' # Import data
#' data (FlyEast)
#'
#'
#' # Create quick summary of the recording with adjustment for daylight saving.
#' BdfList <-
#'     BriefSum (
#'         df = FlyEast,
#'         SR = 1 / 60,
#'         Start = "2017-10-24 13:45:00"
#'     )
#'
#' # Let's extract actigraphy data from a single day
#' df <- BdfList$df
#' df <- subset (df, df$Date == "2017-10-27")
#'
#' fit <- CosinorM (
#'     time = df$Time,
#'     activity = df$Activity,
#'     tau = 24,
#'     method = "OLS"
#' )
#'
#' # Compute variance and Delta SEs
#' res <- se.CosinorM (object = fit)
#' print (res)
#' }
#'
#' @noRd


se.CosinorM <- function (object, method = "delta") {
    # Extract Essential Parameters ------------------
    VCOV <- object$vcov
    SE <- sqrt (diag (VCOV))
    tau <- object$tau
    lT <- length (tau)
    Coefs <- object$coef.cosinor


    ### Collect the betas and gammas
    CSidx <- grep ("^[CS][0-9]+$", row.names (VCOV))

    # Compute Variance for Acrophase and Amplitude  --------------
    VARs <-
        lapply (1:lT, function (i) {
            T <- tau [i]
            VarB <- VCOV [paste0 ("C", i), paste0 ("C", i)]
            VarG <- VCOV [paste0 ("S", i), paste0 ("S", i)]
            CovBG <- VCOV [paste0 ("C", i), paste0 ("S", i)]

            amp <- Coefs [grep (paste0 ("Amplitude.", T), names (Coefs))]

            phi <- Coefs [grep (paste0 ("Acrophase.", T), names (Coefs))]

            beta <- Coefs [grep (paste0 ("Beta.", T), names (Coefs))]

            gamma <- Coefs [grep (paste0 ("Gamma.", T), names (Coefs))]


            ## Amplitude ----------------------
            dA_dB <- beta / amp
            dA_dG <- gamma / amp

            #### Variance
            VarAmp <- dA_dB^2 * VarB +
                2 * dA_dB * dA_dG * CovBG +
                dA_dG^2 * VarG

            ## Acrophase ----------------------
            dP_dB <- beta / (amp^2)
            dP_dG <- -gamma / (amp^2)

            #### Variance
            VarPhi <- dP_dG^2 * VarG +
                2 * dP_dG * dP_dB * CovBG +
                dP_dB^2 * VarB


            x <- c (VarB, VarG, VarAmp, VarPhi)
            names (x) <- c (
                paste0 ("var.Beta.", T),
                paste0 ("var.Gamma.", T),
                paste0 ("var.Amplitude.", T),
                paste0 ("var.Acrophase.", T)
            )

            return (x)
        })


    # Prepare for Output --------------------
    ### Add MESOR
    MESOR <- as.numeric (VCOV [1, 1])
    names (MESOR) <- "var.MESOR"
    VARs [[lT + 1]] <- MESOR

    ### Unwrap list
    VARs <- unlist (VARs)
    SEs <- sqrt (VARs)
    names (SEs) <- gsub ("var", "se", names (SEs))


    ### Reorder to match coef.cosinor
    VARs <- VARs [order (match (gsub ("^var\\.", "", names (VARs)), names (Coefs)))]
    SEs <- SEs [order (match (gsub ("^se\\.", "", names (SEs)), names (Coefs)))]

    return (list (var = VARs, se = SEs))
}
