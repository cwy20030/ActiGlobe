# Summarize the Actigraphy Recording by Day

\`BriefSum()\` is a function that summarizes the actigraphy recording by
day. It generates a data.frame where each row holds all metadata for one
recording day: the calendar date, time–zone code, epoch length
(seconds), UTC offset, daylight–saving flag, cumulative start/end
seconds from midnight, recording start/end times (HH:MM:SS), any warning
labels (e.g. \`Travel\`, \`Incomplete Recording\`), an exclusion flag,
and the expected number of epochs for a full day.

## Usage

``` r
BriefSum(df, SR, Start, TZ = "local")
```

## Arguments

- df:

  A data.frame of raw actigraphy recording. Both time and activity count
  should be included in the \`df\`. See \`VAct\` and \`VTm\` for further
  detail.

- SR:

  The sampling rate of the actigraphy (unit at Hz). Note that Hz should
  be equal to or less than 1.

- Start:

  The starting date and time of the recording in the format as
  "2021-03-05 18:31:03". See
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html) for more
  details and
  [`DateFormat`](https://cwy20030.github.io/ActiGlobe/reference/DateFormat.md)
  and
  [`TimeFormat`](https://cwy20030.github.io/ActiGlobe/reference/TimeFormat.md)
  for formatting help.

- TZ:

  The time zone when the recording started. (default = "local", which
  means user's local time zone)

## Value

A named list with two elements:

- Bdf A data.frame containing brief summary information of each
  recording day. Columns include:

  - Date calendar date (YYYY-MM-DD)

  - Epoch epoch length in seconds

  - UTC dominant UTC offset string (e.g., "UTC+02:00")

  - TZ_code time zone code (e.g., "EST")

  - Daylight_Saving logical flag indicating DST for the day

  - Recording_Start earliest recorded time for the day (HH:MM:SS)

  - Recording_End latest recorded time for the day (HH:MM:SS)

  - GL_Offset numeric offset returned by DST2GL() for the day

  - nDataPoints number of epochs observed for the day

  - Cumulative_Start_Second cumulative start second from midnight for
    the day's first epoch

  - Cumulative_End_Second cumulative end second from midnight for the
    day's last epoch

  - Excluded logical flag; TRUE if the day is excluded (e.g.,
    incomplete)

  - Warning character; warning label when applicable (e.g., "Incomplete
    Recording", "Time Change")

- df The original input df augmented with additional columns (class
  ActiGlobe, data.frame):

  - DateTime POSIXct timestamp for each epoch (tz = TZ)

  - Date date string (YYYY-MM-DD)

  - Time time string (HH:MM:SS)

  - UTC UTC offset string for each epoch

  - DaylightSaving logical flag per epoch

  - nPoint cumulative epoch index

The function returns a list with both Bdf df = df) and sets classes
c("ActiGlobe","data.frame") on both returned data.frames.

## Examples

``` r
if (FALSE) { # \dontrun{
# Import data
data (FlyEast)

BdfList <- BriefSum (
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-19 13:45:00"
)

str (BdfList)
# View(BdfList)

## install library "zeallot"
## library(zeallot)
## c(Bdf, df) %<-%
## BriefSum(df = FlyEast,
##          SR = 1/60,
##          Start = "2017-10-24 13:45:00")
} # }
```
