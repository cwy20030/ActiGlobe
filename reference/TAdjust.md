# Adjust Time Shift based on Travel Log

\`TAdjust()\` is a function that corrects data points and time-shift
based on travelling log. Note that it is important to ensure that the
UTC-offset value is correct. When in doubt, please use the UTC function
or consult IANA table in the package. Daylight saving will not be
reassessed as in BriefSum function.

## Usage

``` r
TAdjust(Bdf, TLog, TZ = NULL, fork = FALSE)
```

## Arguments

- Bdf:

  A
  [`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
  object containing per-day metadata for the recording.

- TLog:

  A structured travel log containing date of travel and local time zone.
  Use \`TravelLog()\` to generate template.

- TZ:

  The time zone when the recording started. (default = "NULL", which
  will disregard the use of the initial geographical location-based time
  zone indicator)

- fork:

  Logical, if TRUE, it will use parallel processing to speed up the
  computation. Default is FALSE.

## Value

A
[`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
object with adjusted data points and time shift based on travel log.

## See also

[`TravelLog`](https://cwy20030.github.io/ActiGlobe/reference/TravelLog.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Import sample data
data (FlyEast)

# Create quick summary of the recording with adjustment for daylight saving.
BdfList <- BriefSum (
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-19 13:45:00"
)

# Extract only the summary report
Bdf <- BdfList$Bdf

# Import sample travel Log
data (TLog)

# Adjust time shift based on travel log
Bdf.adj <- TAdjust (Bdf, TLog)

# Display the summary
View (Bdf)
View (Bdf.adj) ### Focus on the dates after 2017-11-01
} # }
```
