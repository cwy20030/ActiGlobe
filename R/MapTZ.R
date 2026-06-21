#  File ActiGlobe/R/MapTZ.R
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
#' @title Build DST grouping table and leaflet map
#'
#' @description
#' This function builds a table grouping IANA time zones by their UTC offset
#' and DST transition dates/times for a specified year. If provided with a
#' \code{sf} object of time zone polygons, it also generates a leaflet map
#' coloring zones by their DST characteristics.
#'
#'
#' @importFrom leaflet leaflet addTiles addPolygons colorFactor setMaxBounds
#' @importFrom leaflet leafletOptions
#' @importFrom viridis turbo
#' @importFrom sf st_as_sf
#'
#'
#' @param Year Integer year for DST calculation. Default is "Current", which
#' uses the current year of the system.
#' @param Table Logical; whether to return the DST grouping table
#' (default = FALSE).
#'
#' @return list with:
#'   - table: grouped TZ table
#'   - map: leaflet map (if sfTZ supplied)
#'
#'
#' @seealso \code{\link{Geo2TZ}} \code{\link{TZwDST}} \code{\link{TZ2UTC}}
#'
#'
#' @examples
#' # Build DST table and map for all time zones in 2024
#' if(interactive()) {
#'    MapTZ(Year = 2024) # Just the map without table
#' }
#'
#' @keywords map tz country plot
#' @export

MapTZ <- function (Year = "Current", Table = FALSE) {

  # Step 0. Input Validation and Parameter Extraction --------
  if (Year == "Current")
    Year <- as.integer (format (Sys.Date (), "%Y"))

  # Step 1. Build TZ DST table for all IANA zones -------------
  TZtbl <- asNamespace ("ActiGlobe") [[paste0 ("TZDST", Year)]]

  NoDST <- is.na (TZtbl$dstDate) &
           is.na (TZtbl$stDate)  &
           is.na (TZtbl$tfDate)

  # Step 2. Aggregate Transitions for the Map -----------------
  # To avoid duplicating spatial polygons and dynamically capture varying
  # transition rows, we must aggregate the data by time zone beforehand.

  TZList <- split (TZtbl, TZtbl$TZ)

  TZtbl.stnd   <- do.call (rbind,
                       lapply (TZList, function (df) {
    .zTZtbl (df)
  })
  )

  rownames (TZtbl.stnd) <- NULL

  # Step 3. Generate Color Palette ----------------------------
  groups <- unique (TZtbl.stnd$GrpKey [!is.na (TZtbl.stnd$GrpKey)])
  pal    <- leaflet::colorFactor (
    palette = viridis::turbo (length (groups)),
    domain = groups
  )
  TZtbl.stnd$group_color <- pal (TZtbl.stnd$GrpKey)


  # Step 4. Join sfTZ with Aggregated Table ------------------
  sfTZ  <- asNamespace ("ActiGlobe") [["TZsf"]]

  TZcol <- NULL
  for (cand in c ("tzid", "TZID", "tz_name", "tz", "zone", "wof.name")) {
    if (cand %in% names (sfTZ)) {
      TZcol <- cand
      break
    }
  }
  if (is.null (TZcol)) stop ("No IANA TZ column found in sfTZ (sfTZ).")

  sfTZ$TZ <- sfTZ [[TZcol]]

  # Merge with the standardized data (1 row per TZ) avoid polygon duplication
  sfTZ <- merge (sfTZ, TZtbl.stnd, by = "TZ", all.x = TRUE)
  sfTZ <- sf::st_as_sf (sfTZ) # Restore 'sf' class


  # Step 5. Build Popups and Map ------------------------------
  # Using our newly compiled 'Transitions' HTML column
  sfTZ$popup <- sprintf (
    "<b>%s</b><br/>UTC: %s<br/>%s",
    sfTZ$TZ,
    sfTZ$UTC,
    sfTZ$Transitions
  )



  map_obj <- leaflet::leaflet (sfTZ,
                               options = leaflet::leafletOptions (
                                 minZoom = 2,
                                 maxBoundsViscosity = 1.0
                               )) |>
    leaflet::addTiles () |>
    leaflet::addPolygons (
      fillColor = ~ifelse (is.na (GrpKey), "#cccccc", group_color),
      fillOpacity = 0.7,
      color = "#444444",
      weight = 0.2,
      popup = ~popup
    ) |>
    leaflet::setMaxBounds (
      lng1 = -180, lat1 = -90,
      lng2 =  180, lat2 =  90
    )

  # Step 6. Return ------------------------------------
  if (Table) {
    return (list (table = TZtbl,
                  map   = map_obj))
  } else {
    return (map_obj)
  }
}








#' @title Normalize time zone table for leaflet mapping
#'
#' @param data Data frame containing stUTC, dstUTC, and t2UTC columns with UTC
#' offsets, as well as corresponding date and time columns for DST transitions.
#'
#' @return Data frame with columns:
#' \itemize{
#'  \item TZ_NAME: Time zone name
#'  \item UTC: Normalized UTC offset (maximum among stUTC, dstUTC
#'  and t2UTC)
#'  \item Transitions: HTML string summarizing DST transitions
#'  \item GrpKey: Composite key for grouping zones with identical DST
#'  patterns
#'  \item group_color: Color assigned based on GrpKey (added later in MapTZ)
#' }
#'
#'
#' @noRd

.zTZtbl <- function (data) {
  # Normalize UTC  -------------
  UTC_num <- .zUTC (data, Type = "STD")


  # Filter for rows that actively contain transition data -------
  valid_trans <- data [!is.na (data$dstDate) | !is.na (data$stDate) |
                       !is.na (data$tfDate)  | !is.na (data$t2Date), ]

  if (nrow (valid_trans) == 0) {
    trans_html <- "No DST transitions"
    grp_key    <- NA_character_
  } else {

    dk <- rbind (
      unname (as.matrix (
        valid_trans [, c ("TZ", "dstDate", "dstTime", "dstUTC")])),
      unname (as.matrix (
        valid_trans [, c ("TZ", "stDate", "stTime", "stUTC")])),
      unname (as.matrix (
        valid_trans [, c ("TZ", "tfDate", "tfTime", "tfUTC")]))
    )



    # Build multiple rows of transitions into a single HTML string
    Lines <- na.omit (apply (dk, 1, function (row) {
      if (!is.na (row [2])){
      sprintf ("&bull; %s: %s %s",
               row [4], row [2], row [3])
      } else {
        NA_character_
      }
    })
    )


    trans_html <- paste (Lines, collapse = "<br/>")

    # Build a composite group key. This ensures regions with the EXACT
    # same multiple transitions get grouped under the same color pattern.
    keys    <- paste (UTC_num,
                      dk [,2], dk [,3], dk [,4], sep = " | ")
    grp_key <- paste (keys, collapse = " || ")
  }


  # Return ----------
  data.frame (
    TZ               = data$TZ [1],
    UTC              = Num2UTC (UTC_num),
    Transitions      = trans_html,
    GrpKey           = grp_key,
    stringsAsFactors = FALSE
  )


}





#' @title Normalize UTC offsets to numeric for comparison
#'
#' @param data Data frame containing stUTC, dstUTC, and t2UTC
#' columns with UTC offsets
#' @param Type Character string indicating the type of UTC offset to normalize.
#' \itemize{
#' \item STD standard time UTC offsets (stUTC)
#' \item NonSTD prioritize non-standard time UTC offsets (dstUTC and t2UTC)
#' }
#'
#' @return Numeric value representing the maximum UTC offset among the
#' provided columns, normalized to a consistent numeric format for comparison.
#'
#' @noRd

.zUTC <- function (data, Type = c ("STD", "NonSTD")) {
  # Normalize UTC offset to numeric
  ST_UTC <- vapply (data$stUTC, function (x) {
    if (is.na (x)){
      return (NA_real_)
    } else {
      UTC2Num (x)
    }
  },
  FUN.VALUE = numeric (1))

  Out <- ST_UTC


  if (Type == "NonSTD") {
    DST_UTC <- vapply (data$dstUTC, function (x) {
      if (is.na (x)){
        return (NA_real_)
      } else {
        UTC2Num (x)
      }
    },
    FUN.VALUE = numeric (1))



  TS_UTC <- vapply (data$t2UTC, function (x) {
    if (is.na (x)){
      return (NA_real_)
    } else {
      UTC2Num (x)
    }
  },
  FUN.VALUE = numeric (1))


  Out <- max (c (ST_UTC, DST_UTC, TS_UTC), na.rm = TRUE)
  }


  return (Out)
}
