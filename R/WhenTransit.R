#  File ActiGlobe/R/WhenTransit.R
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
#' @title Helper to find transition time between two instants
#'
#' @description
#' Compute the transition time for the daylight saving practice.
#'
#'
#' @param TZ Character. Time zone name.
#' @param Year The character scalar containing the year.
#'
#'
#' @inheritSection .FormatTransitOutput return
#'
#'
#' @examples
#' # Find the DST transition time for Eastern Standard Time
#' WhenTransit(TZ = "America/New_York")
#'
#' # Find the DST transition time for Central European Time
#' WhenTransit(TZ = "Asia/Tehran", Year = 2009)
#'
#' # Find the DST transition time for Western Samoa in 2011
#' WhenTransit(TZ = "Pacific/Apia", Year = 2011)
#'
#' @noRd

WhenTransit <- function (TZ, Year = NULL, ...) {
  # Step 0. Input Validation, Parameter, Null Output Preparation ------
  TZ <- ValInput (TZ, type = "TZ")

  if (is.null (Year)) {
    Year <- as.integer (format (Sys.Date (), "%Y"))
  } else {
    Year <- as.numeric (Year)
  }

  ##########  Special Case  ##########
  if (TZ == "Antarctica/Vostok" && Year == 1994)
    TZ <- "Antarctica/Davis"
  ##########  Special Case  ##########

  ## Year boundary
  Yini <- sprintf ("%d/01/01", Year)
  Yend <- sprintf ("%d/01/01", Year + 1)

  ## Fall back for special cases
  Yini_bk <- sprintf ("%d/12/31", Year - 1)
  Yend_bk <- sprintf ("%d/01/02", Year + 1)


  ## Handle leap year
  Ln <- 365L +
    as.integer((Year %% 400L == 0L) |
                 ((Year %% 4L == 0L) &
                    (Year %% 100L != 0L)))


  ## Null Output -----------------
  DJ.From <- NA_character_
  DJ.To   <- NA_character_
  DJ.Time <- rep (NA_character_, 2)
  DJ.UTC  <- rep (NA_character_, 2)

  TransDST <-  matrix (data = c("DST", NA, NA, NA,
                                "ST",  NA, NA, NA),
                       nrow = 4)


  # Step 1. Compute Sequential Dates -----------
  ## Reference
  Yref   <- seq (from = as.Date (Yini, tz = "UTC"), length.out = Ln)

  ## TZ-adjusted start and end dates
  ystart <- tryCatch(as.POSIXlt (Yini, tz = TZ),
                     error = function (e){
                       as.POSIXlt (Yini_bk, tz = TZ)
                     })
  yend   <- tryCatch(as.POSIXlt (Yend, tz = TZ),
                     error = function (e){
                       as.POSIXlt (Yend_bk, tz = TZ)
                     })

  ### Check if timezone is historical or canonical
  if (is.na (ystart) || is.na (yend)) stop ("Invalid timezone")

  ### Add standard time zone for nonDSTcases
  UTC1 <- Num2UTC (.offset2Num (as.POSIXct (ystart, tz = TZ)))
  TransDST [4,2] <- UTC1

  ### DST
  Days <- seq (from = ystart, to = yend, by = "1 day")
  UTCs <- .offset2Num (Days)
  dUTC <- diff (UTCs)
  Ds   <- DateFormat (Days)

  Time <- format (Days,"%H:%M:%S")
  Time <- C2T (Time, Discrete = TRUE)
  pDST     <- c (0, diff (Time))
  ToSearch <- which (pDST != 0)
  D2chkDST <- Ds [ToSearch]
  if (length (D2chkDST) > 0) {
    D2chkDST <- DateFormat (c (D2chkDST - 1, D2chkDST, D2chkDST + 1))
  }


  # Step 2. Identify Missing Days and Transition Days -----------
  if (any (!all (Yref %in% Ds), length (D2chkDST) > 0)) {
    HalfD <- seq (from = ystart, to = yend, by = "12 hours")
    Ds    <- unique (DateFormat (HalfD))

    ## Step 2.1. Identify Day Jump or DST  -----------------
    Ddiff <- c (1, diff (Ds))
    D2chk <- Ds [which (Ddiff > 1 | Ddiff == 0)]

    DJorDST <- which (D2chk %in% Ds [which (Ddiff > 1)])
    D2chk [DJorDST] <- D2chk [DJorDST] - 1

    ## Step 2.1.1. Extract Day Jump  -----------------
    Dmiss  <- !Yref %in% Ds

    if (any (Dmiss)) {
      DJ.From <- Yref [Dmiss]
      DJ.To   <- DJ.From + 1
      DJ.Time <- c ("00:00:00", "00:00:00")
      DJ.UTC  <- c (
        Num2UTC (.offset2Num (as.POSIXct (DJ.From, tz = TZ))),
        Num2UTC (.offset2Num (as.POSIXct (DJ.To, tz = TZ)))
      )
    }

    ## Step 2.2. Extract DST -----------
    D2chk <- unique (c (D2chk, D2chkDST))
    hsDST <- D2chk [!D2chk %in% DJ.From]
    hsDST <- as.Date (hsDST)

    ## Step 2.2.1. Identify Transition Time -----------
    if (length (hsDST) > 0) {
      TransDST <- vapply (hsDST, function (x)
        .WhenTS (Date = x, TZ = TZ),
        FUN.VALUE = character (4))
    }
  }

  # Step 3. Prepare Output -----------
  ## Check internal
  dots <- list (...)
  Internal <- dots$Internal %||% FALSE   # hidden flag

  Out <- .FormatTransitOutput (DJ.From  = DJ.From,
                               DJ.To    = DJ.To,
                               DJ.Time  = DJ.Time,
                               DJ.UTC   = DJ.UTC,
                               TransDST = TransDST,
                               Year     = Year,
                               TZ       = TZ,
                               dUTC     = dUTC,
                               UTCs     = UTCs,
                               Internal = Internal)


  return (Out)
}








#' @title Helper to find time shift of daylight saving time or other transition
#'
#' @description
#' Helper to find the exact transition time of daylight saving time (DST)
#' for a given date.
#'
#'
#' @param Date A character scalar of pre-identified DST date of class
#'  \code{Date}, \code{POSIXct}, or \code{POSIXlt}.
#' @param TZ A character string of pre-validated time zone.
#' @param TSFrom A logical scalar; if TRUE, returns original date time
#' information before time shift.
#'
#'
#' @return POSIXct of the transition time (local).
#'
#'
#' @examples
#' # Find the DST transition time for Eastern Standard Time
#' .WhenTS(Date = "2026-03-08",TZ = "America/New_York")
#'
#' .WhenTS(Date = "2026-11-01",TZ = "America/New_York")
#'
#' # Find the DST transition time for Western Samoa in 2010
#' .WhenTS(Date = "2010-09-26", TZ = "Pacific/Apia")
#'
#' @noRd

.WhenTS <- function (Date, TZ, TSFrom = FALSE) {

  # Step 0. Input Validation and NUll Output Prepareation ------
  Out <- c (NA_character_, NA_character_, NA_character_, NA_character_)
  if (is.null (Date) || length (Date) == 0 || all (is.na (Date))) {
    return (Out)
  } else {

    Date <- as.Date (Date)

    # Step 1. Prepare candidate times around the date boundary ------
    T0  <- as.POSIXct (paste (Date - 1, "20:00:00"), tz = TZ)
    T1  <- as.POSIXct (paste (Date, "05:00:00"), tz = TZ)

    T01 <- seq (from = T0, to = T1, by = "1 min")

    # Step 2. Check DST or Permanent shift status at T0 and T1 -------------
    A  <- as.POSIXlt (T01, tz = TZ)$isdst
    hsDST <- any (A)
    if (!hsDST) A  <- .offset2Num(T01)
    A  <- c (0, diff (A))
    ID <- which (!A == 0)

    if (length (ID) == 0) {
      return (Out)

    } else {

      U0 <- .offset2Num(T01[ID - 1])
      U1 <- .offset2Num(T01[ID])

      # Step 3 Extract Transition Time ------------------------
      T2chk <- T01 [ID - 1]
      if (all (!TSFrom, !hsDST))
        T2chk <-T01 [ID]


      ChckP <- !grepl("\\b\\d{2}:\\d{2}\\b", T2chk)

      if (any (ChckP)) {
        x     <- DateFormat (T2chk [ChckP], as.date = FALSE)
        T2chk <- format (T2chk, paste (x, "%H:%M:%S"))
      }


      YMD   <- DateFormat (T2chk)
      HMS   <- TimeFormat (T2chk, as.time = TRUE)

      Factor <- ifelse (!TSFrom & !hsDST, 0, 60)
      fnlDT  <- as.POSIXct (paste (YMD, HMS), tz = "UTC") + Factor

      # Step 4 Extract Transition Date ------------------------
      Tdst <- format (fnlDT, "%H:%M:%S")
      YMD  <- DateFormat (fnlDT)



      # Step 5 Prepare Output ------------------------
      Out <- c (paste(YMD), paste (Tdst))

      if (TSFrom & hsDST)
        Out <- rep (NA_character_, 2)

      Type <- ifelse (hsDST,
                      ifelse (TSFrom,
                              ifelse (A[ID] < 0, "DST",
                                      ifelse (A[ID] > 0, "ST", NA_character_)),
                              ifelse (A[ID] < 0, "ST",
                                      ifelse (A[ID] > 0, "DST", NA_character_))),
                      "TS")

      UTCs <- ifelse (TSFrom, Num2UTC (U0),
                      ifelse (Type %in% c ("DST", "ST", "TS"), Num2UTC (U1),
                              NA_character_))


      return (c (Type, Out, UTCs)) # Return transition time
    }
  }
}







#' @title Helper to format and filter timezone transition data structures
#'
#' @description
#' Formats day jump (DJ) and daylight saving time (DST) transition matrices
#' into structured data frames or custom list structures depending on the
#' execution schema flag. Handles standard time fallbacks, removes
#' out-of-boundary year bounds, and processes final table combinations.
#'
#'
#' @param DJ.From Character vector/Date indicating the starting boundary
#' of a day jump.
#' @param DJ.To Character vector/Date indicating the concluding boundary
#' of a day jump.
#' @param DJ.Time Character vector containing timestamp text arrays
#' for day jumps.
#' @param DJ.UTC Character vector specifying the calculated UTC offsets
#' for day jumps.
#' @param TransDST A character matrix containing discovered transition details.
#' @param Year Numeric scalar or character indicating the active target year.
#' @param TZ Character scalar representing the validated time zone name.
#' @param dUTC Numeric vector holding daily differences in UTC offsets.
#' @param UTCs Character scalar representing the year round
#' time offset code.
#' @param Internal Optional indicator for output structur. If TRUE,
#' returns a nested list of tokenized DJ/DST metadata matrices. If
#' FALSE (default), returns a consolidated data frame of localized
#' transition instances.
#'
#'
#' @return
#' A data frame or a structured list containing the formatted transition
#' information for day jumps and daylight saving time changes, filtered
#' by the specified year and time zone, and adjusted for any special cases
#' as needed.
#'
#' When set to Internal = TRUE, the function returns a list with the
#' following structure:
#' \itemize{
#'  \item Ln: The maximum number of transitions (day jumps or DST) found
#'  in the data.
#'  \item DJ: A list containing details of day jumps, including:
#'  \itemize{
#'   \item Ln: The number of day jump transitions.
#'   \item From: A list with UTC, Date, and Time vectors for the start of
#'   day jumps.
#'   \item To: A list with UTC, Date, and Time vectors for the
#'   end of day jumps.
#'   }
#'   \item DST: A list containing details of daylight saving time transitions,
#'   including:
#'   \itemize{
#'   \item Ln: The number of DST transitions.
#'   \item DST: A list with UTC, Date, and Time vectors for the start of DST.
#'   \item ST: A list with UTC, Date, and Time vectors for the
#'   end of DST (standard time).
#'   }
#'   }
#'
#'
#' If Internal = FALSE, the function returns a data frame with columns:
#' \itemize{
#' \item Type: The type of transition (e.g., "TSFrom", "TSTo", "DST", "ST").
#' \item Date: The date of the transition in "YYYY-MM-DD" format.
#' \item Time: The time of the transition in "HH:MM:SS"
#' \item UTC: The UTC offset at the time of the transition (e.g., "UTC -05:00").
#' }
#'
#'
#'
#' @noRd

.FormatTransitOutput <- function(DJ.From, DJ.To, DJ.Time, DJ.UTC,
                                 TransDST, Year, TZ, dUTC, UTCs,
                                 Internal = FALSE) {

  # Step 1. Prepare DJ Output -----------
  dfDJ <- data.frame (
    Type = c ("TSFrom", "TSTo"),
    Date = as.character (c (DJ.From, DJ.To)),
    Time = DJ.Time,
    UTC  = DJ.UTC
  )

  dfDJ  = subset (dfDJ,  subset = !is.na (dfDJ$Type))


  # Step 2. Prepare DST Output -----------
  ## Step 2.1. Check Year Boundary
  Ys <- as.numeric (format (as.Date (TransDST [2,]), "%Y"))
  ToRm <- which (Ys != Year & !is.na (Ys))
  if (length (ToRm) > 0)
    TransDST <- TransDST[, -ToRm, drop = FALSE]


  ## Step 2.2. Check TransDST output
  ChkPoint <- c ("DST", "ST") %in% TransDST[1,]

  if (!all (ChkPoint)) {
    x <- c ("DST", "ST")
    N <- length (x [!ChkPoint])
    TransDST <- cbind (TransDST,
                       matrix (data = c (x [!ChkPoint],
                                         rep (NA_character_, N),
                                         rep (NA_character_, N),
                                         rep (NA_character_, N)),
                               nrow = 4, byrow = TRUE))
  }

  dfDST <- data.frame (t (TransDST))
  names (dfDST) <- c ("Type", "Date", "Time", "UTC")
  dfDST = subset (dfDST, subset = !is.na (dfDST$Type))

  # Step 3. Reorganize and Confirm Output Structure -----------
  ## Step 3.1. Manage Time Zone Shift and DST -----------
  if (any (dfDST$Type == "TS")) {
    TSTo   <- dfDST [dfDST$Type == "TS", ]
    TSFrom <- vapply (TSTo$Date, function (x) {
      if (TSTo$Time [TSTo$Date == x] > "05:00:00")
        x <- as.Date (x) + 1
      .WhenTS (Date = x, TZ = TZ, TSFrom = TRUE)},
      FUN.VALUE = character (4))
    TSFrom <- setNames (as.data.frame (t (TSFrom)),
                        c ("Type", "Date", "Time", "UTC")
    )

    Nrp  <- length (TSFrom$Type)
    dfTS <- data.frame (
      Type = rep (c ("TSFrom", "TSTo"), each = Nrp),
      Date = as.character (c (TSFrom$Date, TSTo$Date)),
      Time = c (TSFrom$Time, TSTo$Time),
      UTC  = c (TSFrom$UTC, TSTo$UTC)
    )
    dfDJ  <- rbind (dfDJ, dfTS)
    dfDJ  <- subset (dfDJ, subset = !is.na (dfDJ$UTC))
    dfDST <- subset (dfDST, subset = !dfDST$Type == "TS")
  }


  ## Step 3.2. Amend south pole time change -----------
  utcDST <- dfDST$UTC [dfDST$Type == "DST"]
  utcST  <- dfDST$UTC [dfDST$Type == "ST"]
  if (all (is.na (utcST)))
    dfDST$UTC [dfDST$Type == "ST"] <- Num2UTC (min (UTCs))


  # if (all (is.na (utcDST)) & any (dUTC < 0))
  #  dfDST$UTC [dfDST$Type == "DST"] <- Num2UTC (max (UTCs))


  ## Step 3.3. Check repeated values ----------
  utcST  <- dfDST$UTC [dfDST$Type == "ST"]
  # utcDST <- dfDST$UTC [dfDST$Type == "DST"]

  if (all (na.omit (NA_equal (utcDST, utcST))))
    dfDST$UTC[dfDST$Type == "DST"] <- NA


  # Step 4. Validation of Inclusion of all UTC offsets ----------------
  dfDST <- .ValTransit (dfDST, dfDJ, UTCs, TZ)

  # Step 4. Organize Output ----------------------
  if (!Internal) {
    Out <- as.data.frame (rbind (dfDJ, dfDST))
  } else {
    Ln   <- max (table (c (dfDJ$Type, dfDST$Type)))
    if (Ln %% 2 == 1)
      Ln <- Ln - 1 # Ensure even number of transitions for pairing

    Out <- list (
      Ln = Ln,

      DJ = list (Ln   = round (length (dfDJ$Type) / 2),
                 From = list (UTC  = dfDJ$UTC  [dfDJ$Type == "TSFrom"],
                              Date = dfDJ$Date [dfDJ$Type == "TSFrom"],
                              Time = dfDJ$Time [dfDJ$Type == "TSFrom"]),
                 To   = list (UTC  = dfDJ$UTC  [dfDJ$Type == "TSTo"],
                              Date = dfDJ$Date [dfDJ$Type == "TSTo"],
                              Time = dfDJ$Time [dfDJ$Type == "TSTo"])),

      DST = list (Ln  = round (length (dfDST$Type) / 2),
                  DST = list (UTC  = dfDST$UTC  [dfDST$Type == "DST"],
                              Date = dfDST$Date [dfDST$Type == "DST"],
                              Time = dfDST$Time [dfDST$Type == "DST"]),
                  ST  = list (UTC  = dfDST$UTC  [dfDST$Type == "ST"],
                              Date = dfDST$Date [dfDST$Type == "ST"],
                              Time = dfDST$Time [dfDST$Type == "ST"]))
    )
  }
  return (Out)
}






#' @title Helper to Validate Input Based on the UTCs
#'
#' @description
#' This function checks for any missing UTC offsets in the provided data frames
#' and adds necessary entries to the DST data frame if there are missing
#' offsets that should be accounted for.
#'
#' @param dfDST A data frame containing daylight saving time transition
#' information.
#' @param dfDJ A data frame containing time shift transition information.
#' @param UTCs A character vector of UTC offsets that should be present in the
#' data frames.
#' @param TZ A character scalar representing the time zone for which the
#' validation is being performed.
#'
#' @return A data frame with the necessary DST entries added if there were
#' missing UTC offsets.
#'
#' @noRd

.ValTransit <- function (dfDST, dfDJ, UTCs, TZ) {
  # Step 0. Parameter Extraction ------
  Type <- dfDST$Type
  nType <- ifelse (Type == "DST", 1,
                   ifelse (Type == "ST",0, NA))
  UTC  <- dfDST$UTC
  Date <- dfDST$Date

  tUTC <- dfDJ$UTC

  uOffset  <- unique (UTCs)
  aOffset  <- UTC2Num (na.omit (c (UTC, tUTC)))
  msOffset <- uOffset [!uOffset %in% aOffset]

  # Step 1. Check Missing UTC Offsets and Add DST Entries if Needed ------
  if (length (msOffset) > 0) {

    if (length (na.omit (UTC)) > 1){

    dUTC <- abs (diff (UTC2Num (UTC)))
    dUTC [is.na (dUTC)] <- 0

    if (paste0 (diff (c (0, nType)),collapse = "") == "1-1") {
      dfAdd <-.WhenTS (Date [Type == "DST"], TZ, TSFrom = TRUE)
      dfAdd <- setNames (as.data.frame (t (dfAdd)),
                         c ("Type", "Date", "Time", "UTC"))

      ## Last Sanity Check
      dfAdd <- dfAdd [!dfAdd$UTC  == UTC [Type == "ST"], ]
      dfDST <- rbind (dfAdd, dfDST)

    }

  } else {

  switch (Type [is.na (UTC)],
          "DST" = {
            dfAdd <- .WhenTS (Date [Type == "ST"], TZ, TSFrom = TRUE)
            dfAdd <- setNames (as.data.frame (t (dfAdd)),
                               c ("Type", "Date", "Time", "UTC"))
            dfDST[Type == "DST",] <- dfAdd
          },
          "ST" = {
            dfAdd <- .WhenTS (Date [Type == "DST"], TZ, TSFrom = FALSE)
            dfAdd <- setNames (as.data.frame (t (dfAdd)),
                               c ("Type", "Date", "Time", "UTC"))
            dfDST[Type == "ST",] <- dfAdd
          }

          )
  }
  }




  # Step 2. Return Output ------
  return (dfDST)
}



#' @title Helper to Compare Two Dates for Equality
#'
#' @param x A date object or a character string representing a date.
#' @param y A date object or a character string representing a date.
#'
#' @return A logical value indicating whether the two dates are the same,
#' or NA if either input is NA.
#'
#' @noRd

.SameDate <- function(x, y) {
  if (is.na(x) || is.na(y)) {
    return (NA)
  } else {
    return (as.Date(x) == as.Date(y))
  }
}
