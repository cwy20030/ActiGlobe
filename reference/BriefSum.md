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
  should be included in the `df`.

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

  - GL_Offset numeric offset returned by
    [`DST2GL`](https://cwy20030.github.io/ActiGlobe/reference/DST2GL.md)
    for the day

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
# Import data
data (FlyEast)

BdfList <- BriefSum (
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-19 13:45:00"
)

str (BdfList)
#> List of 2
#>  $ Bdf:Classes 'ActiGlobe' and 'data.frame': 35 obs. of  13 variables:
#>   ..$ Date                   : chr [1:35] "2017-10-19" "2017-10-20" "2017-10-21" "2017-10-22" ...
#>   ..$ Epoch                  : num [1:35] 60 60 60 60 60 60 60 60 60 60 ...
#>   ..$ UTC                    : chr [1:35] "UTC-04:00" "UTC-04:00" "UTC-04:00" "UTC-04:00" ...
#>   ..$ TZ_code                : chr [1:35] "EDT" "EDT" "EDT" "EDT" ...
#>   ..$ Daylight_Saving        : logi [1:35] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ Recording_Start        : chr [1:35] "13:45:00" "00:00:00" "00:00:00" "00:00:00" ...
#>   ..$ Recording_End          : chr [1:35] "23:59:00" "23:59:00" "23:59:00" "23:59:00" ...
#>   ..$ GL_Offset              : num [1:35] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ nDataPoints            : num [1:35] 615 1440 1440 1440 1440 1440 1440 1440 1440 1440 ...
#>   ..$ Cumulative_Start_Second: num [1:35] 60 36960 123360 209760 296160 ...
#>   ..$ Cumulative_End_Second  : num [1:35] 36900 123300 209700 296100 382500 ...
#>   ..$ Excluded               : logi [1:35] TRUE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ Warning                : chr [1:35] "Incomplete Recording" "" "" "" ...
#>  $ df :Classes 'ActiGlobe' and 'data.frame': 48847 obs. of  9 variables:
#>   ..$ Activity      : int [1:48847] 11 0 0 0 0 0 0 0 0 0 ...
#>   ..$ X2            : chr [1:48847] "0.00" "0.00" "0.00" "0.00" ...
#>   ..$ Marker        : int [1:48847] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ DateTime      : POSIXct[1:48847], format: "2017-10-19 13:45:00" "2017-10-19 13:46:00" ...
#>   ..$ Date          : chr [1:48847] "2017-10-19" "2017-10-19" "2017-10-19" "2017-10-19" ...
#>   ..$ Time          : chr [1:48847] "13:45:00" "13:46:00" "13:47:00" "13:48:00" ...
#>   ..$ UTC           : chr [1:48847] "UTC-04:00" "UTC-04:00" "UTC-04:00" "UTC-04:00" ...
#>   ..$ DaylightSaving: logi [1:48847] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ nPoint        : int [1:48847] 1 2 3 4 5 6 7 8 9 10 ...
# View(BdfList)

## install library "zeallot"
## library(zeallot)
## c(Bdf, df) %<-%
## BriefSum(df = FlyEast,
##          SR = 1/60,
##          Start = "2017-10-24 13:45:00")

```
