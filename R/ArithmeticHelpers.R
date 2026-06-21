#  File ActiGlobe/R/SeqGroup.R
#
#  Copyright (C) 2026  C. William Yao, PhD
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
#' @title Group Consecutive Sequential Steps in a Numeric Vector
#'
#' @description
#' rle style grouping function for sequential values
#'
#' @details
#' \strong{Cyclic vs. linear mode}\cr
#' When \code{Wrap = TRUE} (default), a sequence that overshoots \code{Max} is
#' expected to continue from \code{Min} (e.g., compass bearings, hours of the
#' day).
#' When \code{Wrap = FALSE}, no modular wrapping is applied to the expected
#' next value, so only strictly linear progressions are grouped.
#'
#' @param x A vector of sequential values in numeric, POSIXct or PoSIXlt form.
#'
#' @param Step A numeric scalar giving the expected difference between
#'   successive group members.
#'
#' @param Min Numeric scalar: lower bound of the cyclic range.
#'   Defaults to \code{min(x, na.rm = TRUE)}.
#'
#' @param Max Numeric scalar: upper bound (exclusive) of the cyclic range.
#'   Defaults to \code{max(x, na.rm = TRUE)}.
#'
#' @param Wrap Logical scalar. If \code{TRUE}, Lead values are
#'   wrapped into \eqn{\frac{\code{Min}}{\code{Max}}} via modular arithmetic,
#'   enabling detection of sequences that cross the range boundary.
#'   If \code{FALSE} (default), wrapping is disabled and only linear runs are
#'   detected.
#'
#' @param Simple Logical scalar. If \code{TRUE} (default), return only the
#' \code{rle}-style elements \code{lengths} and \code{values}. If \code{FALSE},
#' additional elements \code{mins}, \code{maxs}, and \code{groups} are
#' included in the output list, providing the minimum and maximum values of
#' each group, and the indices of the original vector that belong to each
#' group, respectively.
#'
#' @param tol Non-negative numeric scalar. Two values are considered equal when
#'   their absolute difference is at most \code{tol}. Default is \code{1e-8}.
#'
#' @return A named list with two elements, mirroring \code{\link[base]{rle}}:
#' \describe{
#'   \item{\code{lengths}}{Integer vector. The length of each consecutive
#'     sequential group.}
#'   \item{\code{values}}{Numeric vector. The minimum and maximum values of
#'   each group formated as \code{"min-max"}}.
#'   \item{\code{mins}}{Numeric vector. The minimum value of each group. Only
#'   included if \code{Simple = FALSE}.}
#'   \item{\code{maxs}}{Numeric vector. The maximum value of each group. Only
#'   included if \code{Simple = FALSE}.}
#'   \item{\code{groups}}{List of integer vectors. Each element contains
#'   the indices of \code{x} that belong to the corresponding group. Only
#'   included if \code{Simple = FALSE}.}
#' }
#'
#'
#' @seealso \code{\link[base]{rle}} for run-length encoding of repeated values;
#'   \code{\link{ValInput}} for the upstream input validator.
#'
#' @examples
#' \donttest{
#' # --- Linear stepping (no wrapping needed) ---
#' x1 <- c(1, 1.5, 2, 2.5, 5, 5.5, 6)
#' SeqGroup(x1, Step = 0.5)
#' # $lengths: 4 3
#' # $values : 1 5
#'
#' # --- Cyclic wrapping (e.g., compass bearings 0–360) ---
#' x2 <- c(358, 359, 0, 1, 2, 90, 91)
#' SeqGroup(x2, Step = 1, Min = 0, Max = 360, Wrap = TRUE)
#' # $lengths: 5 2
#' # $values : 358 90
#'
#' # --- Wrapping disabled ---
#' SeqGroup(x2, Step = 1, Min = 0, Max = 360, Wrap = FALSE)
#' # 358→359 continues; 359→0 breaks because 0 ≠ 360 without wrapping
#' }
#' @noRd


SeqGroup <- function (x,
                      Step,
                      Min = min (x, na.rm = TRUE),
                      Max = max (x, na.rm = TRUE),
                      Wrap = FALSE,
                      Simple = TRUE,
                      tol = 1e-8) {

  # Step 0. Check Point and Input Validation -------------------------
  ln <- length (x)
  if (ln == 0) {
    xname <- deparse (substitute (x))
    stop (paste ("Input vector", xname ,"is empty."))
  }

  g <- setNames (
    as.numeric (as.factor (x)),
    x)

  if (any (is.na (Min), is.na (Max))) {
    Min <- min (x, na.rm = TRUE)
    Max <- max (x, na.rm = TRUE)
  }


  # Step 1. Vectorization --------------
  ## Lag
  Lag <- c (NA_real_, x[-ln])

  ## Lead
  Lead <- Lag + Step

  if (Wrap) ### wrap around the Lead values into [Min, Max)
    Lead <- (Lead - Min) %% (Max - Min) + Min

  # Step 2. Check Continuation ------------------------
  ## Flag for continuation
  Flag      <- rep (FALSE, ln)
  Flag [-1] <- abs (x [-1] - Lead [-1]) <= tol


  # Step 3. Store rle-style Output ------------
  ID  <- cumsum (!Flag) # group ids
  Gp  <- split (seq_len (ln), ID)
  Ln  <- vapply (Gp, length, integer (1))
  Ini <- vapply (
    Gp,
    function (idx) min (x [idx], na.rm = TRUE),
    numeric (1)
  )

  End <- vapply (
    Gp,
    function (idx) max (x [idx], na.rm = TRUE),
    numeric (1)
  )

  nDig <- max (0L, -floor (log10 (Step))) # Display digits
  nDf  <- paste0 (rep (
    paste0 ("%.",nDig,"f"), 2), collapse = "-")
  Val <- sprintf (nDf, Ini, End)

  # Step 4. Return everything -------------------
  if (Simple) {
     return (list (lengths = Ln, values = Val))
  } else {
    return (list (lengths         = Ln,
                  values          = Val,
                  mins            = Ini,
                  maxs            = End,
                  groups          = Gp ))
  }

}


#  File ActiGlobe/R/SeqFill.R
#
#  Copyright (C) 2026  C. William Yao, PhD
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
#' @title Align and Fill Observations to a Regular Reference Sequence
#'
#' @description
#' Align a numeric vector of observed sequential values \code{x} to a reference
#' grid. The function maps each observation to its reference index, groups
#' contiguous runs, and produces either a matrix, a linear vectorized fill,
#' or a list containing both representations plus simple metadata.
#'
#'
#' @param x Numeric vector of observed sequential values.
#'
#' @param Step Numeric scalar. The expected increment between successive
#'   reference values (required).
#'
#' @param Format Character scalar. One of \code{c("All", "Vector", "Matrix",
#' "Index)}. Controls the return type. Default order in the function signature
#' is used for argument matching.
#'
#' @param Filler Character or numeric scalar. The value to fill in missing
#' positions. Example of options:
#' \itemize{
#'  \item \code{NA} (default): fill with NA.
#'  \item 0: fill with zero.
#'  \item "Ref": fill with the reference value corresponding to the missing
#'  position.
#'  }
#'
#' @param Min Numeric scalar. Lower bound of the reference range. Defaults to
#'   \code{min(x, na.rm = TRUE)}.
#'
#' @param Max Numeric scalar. Upper bound of the reference range. Defaults to
#'   \code{max(x, na.rm = TRUE)}.
#'
#' @param Gap Optional integer scalar. If provided, any forward jump in mapped
#'   reference indices greater than \code{Gap} is treated as a group break.
#'   If \code{NULL} (default), only non-increasing indices start a new group.
#'
#' @return
#' Depending on \code{Format}:
#' \describe{
#'   \item "Index": An integer vector of reference indices corresponding to
#'   a complete sequential series.
#'   \item "Matrix": A numeric matrix with N rows of repeated sequence
#'     and M columns of maximum data points per day.
#'     Observed values are placed at the column corresponding to their mapped
#'     reference index; missing positions are \code{NA}.
#'   \item "Vector": A numeric vector of NA filled data.
#'   \item "All": A \code{list} with elements:
#'     \itemize{
#'       \item meta: a data.frame with per-group diagnostics
#'         group_id, na_in_group.
#'       \item matrix: the matrix described above.
#'       \item vector: the numeric vector described above.
#'       \item id: the integer vector described above in "Index".
#'     }
#'   }
#'
#' @seealso \code{\link{seq}} for reference sequence construction;
#'   \code{\link{SeqGroup}} for alternative grouping logic.
#'
#' @examples
#' \donttest{
#' # Basic matrix output: align observations to integer grid 0..10
#' x <- c(0, 1, 4, 5, 9)
#' SeqFill(x, Step = 1, Min = 0, Max = 10, Filler = NA, Format = "Matrix")
#'
#' # Request the linearized vector representation
#' SeqFill(x, Step = 1, Min = 0, Max = 10, Filler = NA, Format = "Vector")
#'
#' # All outputs together (meta, matrix, vector)
#' out <- SeqFill(x, Step = 1, Min = 0, Max = 10, Filler = NA, Format = "All")
#' out$meta
#'
#' # Treat a large forward jump as a break
#' x2 <- c(0, 1, 2, 8, 9)
#' SeqFill(x2, Step = 1, Min = 0, Max = 10, Filler = NA, Gap = 3)
#'
#'
#' # Floating point step: tolerance-safe matching
#' x3 <- c(0.0, 0.5, 1.0, 2.0)
#' SeqFill(x3, Step = 0.5, Min = 0, Max = 2, Filler = NA, Format = "Matrix")
#' }
#'
#' @noRd


SeqFill <- function (x,
                     Step,
                     Format = c ("All", "Vector", "Matrix", "Index"),
                     Filler = c (NA, 0, "Ref"),
                     Min = min (x, na.rm = TRUE),
                     Max = max (x, na.rm = TRUE),
                     Gap = NULL,
                     tol = 1e-8) {

  # Step 0. Parameter  extraction -----------
  if (length (Format) > 1L) {
    Format <- "All"
    warning ("Multiple Format options provided. Defaulting to 'All'.")
  }
  if (length (Filler) > 1L & ! toupper (Format) == "INDEX") {
    Format <- NA
    warning ("Multiple object provided for filler. Defaulting to 'All'.")
  }
  if (length (Gap) == 1L & !is.numeric (Gap)) {
    stop ("Gap must be a numeric scalar. Ignoring Gap parameter.")
  }

  Ref <- seq (Min, Max, by = Step)
  nR  <- length (Ref)
  if (is.null (tol)){
    nDig <- max (0L, -floor (log10 (Step)) + 2L) # digits to match
  } else {
    nDig <- nchar (sub (".*\\.", "", format (tol, scientific = FALSE)))
  }


  # Step 1. Position mapping ------------------------
  MatchID <- match (round (x, nDig),
                    round (Ref, nDig))

  ## Stop for unmatched non-referenceable object
  if (anyNA (MatchID)) {
    bad <- is.na (MatchID)
    stop (sum (bad), " value(s) not in reference — dropped")
  }

  # Step 2. Grouping ------------------------
  dx   <- diff (MatchID)
  Brk <- dx <= tol
  if  (!is.null (Gap)) Brk <- Brk | (dx > Gap)
  GID <- cumsum (c (TRUE, Brk))
  Ln  <- max (GID)


  # Step 3. Vectorized Filling (using linear index) ------------------------
  LinID     <- (GID - 1L) * nR + MatchID
    if (!toupper (Format) == "INDEX") {
    if (is.na (Filler)) {
      Nx <- rep (NA_real_, nR * Ln) # Create template
    } else if (toupper (Filler) == "REF") {
      Nx <- rep (Ref, Ln)
    } else if (!is.na (suppressWarnings (as.numeric (Filler)))) {
      Nx <- rep (as.numeric (Filler), nR * Ln)
    } else {
      Nx <- rep (Filler, nR * Ln) # Create template
    }

    Nx [LinID] <- x
  }



  # Step 4. Output ------------------------------
  if (Format %in% c ("All","Matrix"))
    Out  <- matrix (Nx, nrow = Ln, ncol = nR, byrow = TRUE)

  switch (Format,
          "Index" = return (LinID),
          "All" = {
            Meta <- data.frame (
              group_id      = seq_len (Ln),
              na_in_group   = rowSums (is.na (Out))
            )
            return (list (meta   = Meta,
                          matrix = Out,
                          vector = Nx,
                          id     = LinID))
          },
          "Matrix" = return (Out),
          "Vector" = return (Nx)
          )
}




#  File ActiGlobe/R/Mode.R
#
#  Copyright (C) 2026  C. William Yao, PhD
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
#' @title Find Statistical Mode of Input
#'
#' @param x A vector of values for which the mode is to be computed. The object
#' must not be of length zero after optional NA removal.
#'
#' @param na.rm Logical scalar. If \code{TRUE} (default), NA values are removed
#' before computation. If \code{FALSE}, the presence of any NA will result in
#' an NA mode.
#'
#' @param ties Character string specifying how to handle ties (multiple modes):
#' \itemize{
#'  \item "All": Return all modes as a vector
#'  \item "First": Return only the first mode
#'  \item "Random": Randomly select one mode from the tied values
#'  }
#'
#' @param simple Logical scalar. If \code{TRUE} (default), return only the
#' mode(s). If \code{FALSE}, return a list containing the modes, their
#' frequency, and the full frequency table for reference.
#'
#'
#' @returns
#' A vector of mode(s) if \code{simple = TRUE}, or a list with components:
#' \itemize{
#'  \item \code{modes}: The mode value(s) as a vector
#'  \item \code{freq}: The frequency of the mode(s)
#'  \item \code{table}: The full frequency table of all unique values in
#' \code{x}
#' }
#'
#' @examples
#' # Default with print out all the ties
#' Mode(FlyEast_adj$Hour, ties = "All")
#'
#' # Use "First" to return only the first mode
#' Mode(FlyEast_adj$Hour, ties = "First")
#'
#'
#' @noRd

Mode <- function(x,
                 na.rm  = TRUE,
                 ties   = c ("All", "First", "Random"),
                 simple = TRUE) {

  # Step 0. Check Point and Input Validation -------------------------
  ties   <- match.arg (ties)
  xClass <- class (x)

  x <- as.character (x)

  if (na.rm) x <- x [!is.na (x)]
  if (length (x) == 0) {
    xname <- deparse (substitute (x))
    stop (paste ("Input vector", xname ,"is empty."))
  }


  # Step 1. Compute frequency table -------------------
  if (na.rm) {
    Freq <- table (x, useNA = "no")
  } else {
    Freq <- table (x, useNA = "ifany")
  }
  Max <- max (Freq)
  y   <- names (Freq) [Freq == Max] # find values with max frequency


  # Step 2. Handle class assignment ----------------------
  if ( any (xClass %in% c ("POSIXlt", "POSIXct"))) {
    DFormat <- suppressWarnings (
      DateFormat (DT = y, as.date = FALSE)
      )
    TFormat <- suppressWarnings (
      TimeFormat (time = y, as.time = FALSE)
      )

    if (all (DFormat == "", is.na (TFormat))) {
      y <- ValInput (y, type = "DT")
      } else if (all (!DFormat == "", is.na (TFormat))) {
      y <- ValInput (y, type = "Date")
      } else if (all (DFormat == "", !is.na (TFormat))) {
      y <- ValInput (y, type = "Time")
    }
  } else if (length (unique(xClass)) > 1 ) {
    warning (paste0 ("Input has multiple classes. Output will be assigned to
    the first object class - ", xClass [[1]],"."))
    y <- as (y, xClass [[1]])

  } else {

    class (y) <- xClass
  }



  # Step 3. Handle ties ----------------------
  Out <- switch (ties,
                 All    = y,
                 First  = y [1],
                 Random = sample (y, 1))


  # Step 4. Return output ----------------------
  if (simple) {

    return (Out)

  } else {

    return (
      list (modes = Out,
            freq  = as.integer (Max),
            table = Freq
      )
    )
  }

}



#' @title Pad data for edge epochs
#'
#' @param x A vector
#'
#' @param Window Numeric scalar(s). If the input length is one, the function
#' assumes symmetrical window. If the input length is two, the first object
#' will be assumed as the length of the left window; and the second object
#' as of the right window.
#'
#' @param Pad Character scalar. Padding method for edge epochs where the
#' full window overhangs. Available methods include:
#' \itemize {
#'  \item NA (default, no padding): edge epochs return NA
#'  \item "Reflect": pad by reflecting the data at the edges (e.g., y[2], y[3],
#'  ... for left edge)
#'  \item "Zero": pad with zeros at the edges
#' }
#'
#' @examples
#' x <- 1:10
#'
#' # Symmetrical window
#' Padding(x, Window = 3, Pad = NA) # No padding, edge epochs return NA
#' Padding(x, Window = 3, Pad = "Zero") # Zero padding,
#' Padding(x, Window = 3, Pad = "Reflect") # Reflect padding
#'
#' # Asymmetrical window
#' Padding(x, Window = c(2, 3), Pad = NA)
#' Padding(x, Window = c(2, 3), Pad = "Zero")
#' Padding(x, Window = c(2, 3), Pad = "Reflect")
#'
#'
#' @noRd

Padding <- function (x, Window, Pad = NA) {
  ln <- length (Window)
  if (ln > 2) stop ("Window must be of length 1 or 2.")

  if (ln == 1L) {
    Rhw <- Lhw <- Window
  } else {
    Lhw <- Window [[1]]
    Rhw <- Window [[2]]
  }

  n  <- length (x)

  if (is.na (Pad)) {
    # No padding: edge epochs return NA
    x_pad <- c (rep (NA, Lhw), x, rep (NA, Rhw))

  } else if (tolower (Pad) == "zero") {

    Zs <- ifelse (is.integer (x), 0L, 0)
    x_pad <- c (rep (Zs, Lhw), x, rep (Zs, Rhw))

  } else if (tolower (Pad) == "reflect") {
    # Reflect padding: pad by reflecting the data at the edges
    ## reflect left (exclude x[1] = no duplication)
    x_pad <- c (rev (x [2 : (Lhw + 1)]),
                x,
                rev (x [(n - Rhw) : (n - 1)])) # reflect right (exclude x[n])
  }

  return (x_pad)
}
