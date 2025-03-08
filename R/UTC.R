
#' @import hms lubridate
#' @param d The date in formats like "2021-03-05"
#' @param TZ The time zone when the recording started. (default = "local", which means user's local time zone)

UTC = function(d, TZ) {

  d = as.Date(d)
  x = ymd_hms(paste0(d," 12:00:00"), tz = TZ)
  y = with_tz(x, tzone = "UTC")

  a = as.POSIXct(format(x, format = "%H:%M"), format = "%H:%M")
  b = as.POSIXct(format(y, format = "%H:%M"), format = "%H:%M")

  ab = a - b ### Compute time difference
  ab = as.numeric(ab, units = "hours") ### Convert to numeric


  ## Get the hours
  c = ifelse(ab <0, ceiling(ab), floor(ab)) ### Hour unit
  d = (abs(ab) - abs(c)) * 60   ### Minute unit

  mp = ifelse(ab <0, "-", "+") ### Check positive or negative

  e = ifelse(abs(c)<10, paste0(mp,"0",abs(c)), paste0(mp,c))
  f = ifelse(d == 0, "00", d)


  Out = paste0("UTC", e,":",f)


    return(Out)


}
