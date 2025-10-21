# File ActiGlobe/R/plot.ActiGlobe.R
#
# Copyright (C) 2025  C. William Yao, PhD
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Plot an Overview of an `ActiGlobe` Activity Time Series
#'
#' @description
#' Creates a time‐series `scatterplot` of activity counts from an `ActiGlobe` data.frame,marking each midnight boundary with a vertical dashed line and `coloring` points that were flagged (e.g. travel overlaps or unallocated epochs).
#'
#' @import ggplot2
#' @param x A data.frame of annotated actigraphy epochs.  Must include:
#'   - An activity column named by `VAct`.
#'   - A datetime column named by `VDT`.
#'   - Optionally, a `Note` column to flag affected epochs.
#'
#' @param Bdf A BriefSum object containing per‐day metadata for the recording.
#'   If you have applied jet‐lag or daylight‐saving adjustments, pass the output
#'   from the ActiGlobe function `TAdjust`.
#'
#' @param VAct Character. Name of the activity column in `df` (numeric counts).
#'
#' @param VDT Character. Name of the POSIXct datetime column in `df`.
#'
#' @param ... graphical parameters to plot
#'
#' @return A `ggplot` object showing:
#'   - Activity counts vs. time.
#'   - Dashed vertical lines at each midnight.
#'   - Points `colored` by whether they were flagged in `Note`.
#'
#' @examples
#' \dontrun{
#' library(ActiGlobe)
#'
#' # Overview the Uncorrected Longitudinal Recording
#' data(FlyEast)
#'
#' BdfList =
#' BriefSum(df = FlyEast ,
#'          SR = 1/60,
#'          Start = "2017-10-24 13:45:00")
#'
#' p <- plot.ActiGlobe(df   = BdfList$df,
#'                     Bdf  = BdfList$Bdf,
#'                     VAct = "Activity",
#'                     VDT  = "DateTime")
#'
#' print(p)
#'
#'
#'
#' # Overview the Corrected Longitudinal Recording
#' data(TLog)
#'
#' BdfList$Bdf.adj = TAdjust(BdfList$Bdf, TLog)
#' p2 <- plot.ActiGlobe(df   = BdfList$df,
#'                     Bdf  = BdfList$Bdf.adj,
#'                     VAct = "Activity",
#'                     VDT  = "DateTime")
#' print(p2)
#'
#' # Pro-tip: [`cowplot`] can help stack the time series graphs in one single plot
#' }
#'
#' @rdname plot.ActiGlobe
#' @keywords visualization actigraphy
#' @export

plot.ActiGlobe <- function(x, Bdf, VAct, VDT, ...) {


  ## Ensure Note column exists
  if (!"Note" %in% names(df)) {
    df$Note <- ""
  }

  A <- df[[VAct]]
  Mx = round(max(A, na.rm = TRUE))
  mn = round(min(A, na.rm = TRUE))


  DT <- df[[VDT]]
  if (!inherits(DT, c("POSIXct", "POSIXlt"))) {
    DT <- as.POSIXct(DT)
  }

  Nt <- df$Note

  ## Identify midnight boundaries
  MdN <- as.factor(ifelse(grepl("0", DT), "1", "0"))

  ## Flag points with any Note (e.g. travel overlap or unallocated)
  E <- as.factor(ifelse(Nt != "", "1", "0"))

  ggplot2::ggplot(mapping = ggplot2::aes(x = DT, y = A, colour = E)) +
    ggplot2::geom_point(alpha = 0.05) +
    ggplot2::geom_vline(
      xintercept = as.numeric(DT[MdN == "1"]),
      linetype   = "dashed",
      color      = "blue",
      size       = 0.8
    ) +
    ggplot2::scale_y_continuous(limits = c(mn, Mx)) +
    ggplot2::labs(x = "Date", y = "Activity Count") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(0, 0, 0, 0),
      axis.line       = ggplot2::element_line(size = 0.8),
      axis.text       = ggplot2::element_text(color = "black"),
      legend.position = "none"
    )

}
