# Compute Time Gain/Loss due to Daylight Saving Time

DST2GL computes, for each date-time entry, how many hours a day deviates
from the standard 24hours because of DST shifts. Internally it calls
[`Date2TotalT()`](https://cwy20030.github.io/ActiGlobe/reference/Date2TotalT.md)
in seconds, subtracts the 86400s of a normal day, and converts the
remainder to hours.

## Usage

``` r
DST2GL(DT, TZ = "local")
```

## Arguments

- DT:

  A vector of class `Date`, `POSIXct`, or `POSIXlt`, sorted in ascending
  order. Each element marks the start of a day boundary. To capture DST
  transitions, include at least one day before and after the expected
  shift. Note that the time zone should be specified in the DT. See
  `as.POSIXct`.

- TZ:

  The time zone when the recording started. (default = "local", which
  means user's local time zone)

## Value

A numeric vector of the same length as `DT`. Each value is the number of
hours that day is longer (positive) or shorter (negative) than 24h. A
zero means no DST shift on that date.

## Examples

``` r
# Example around a typical spring-forward transition (e.g. US second Sunday in March)
dates <- as.Date ("2021-03-14")

# On 2021-03-14 clocks jumped forward: day is 23h so output = -1
DST2GL (dates)
#> [1] 0


# Example around a fall-back transition (e.g. first Sunday in November)
dates <- as.Date ("2021-11-07")

# On 2021-11-07 clocks fall back: day is 25h so output = +1
DST2GL (dates)
#> [1] 0

# Multiple Dates
sapply (c ("2021-03-13", "2021-03-14", "2021-03-15"), DST2GL)
#> 2021-03-13 2021-03-14 2021-03-15 
#>          0         -1          0 
```
