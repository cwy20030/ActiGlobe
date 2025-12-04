# Compute Total Time in Each Date (Per Day)

For an ordered vector of Dates or date-times, \`Date2TotalT()\`
calculates the interval from each element to the next (and for the last
element, to the following calendar day at midnight) and returns these
durations in the requested time unit.

## Usage

``` r
Date2TotalT(DT, TUnit = "hour", TZ = "local")
```

## Arguments

- DT:

  A vector of class \`Date\`, \`POSIXct\` or \`POSIXlt\`, sorted in
  ascending order. Each entry represents the start of an interval.
  Internally, the function appends one extra day beyond the last entry
  so that the final interval covers a full 24 h until the next midnight.
  See `as.POSIXct`.

- TUnit:

  Character string specifying the unit for the output durations. Must be
  one of \`"hour"\`, \`"minute"\`, or \`"second"\`. Comparison is
  case-insensitive. Default is \`"hour"\`, meaning the returned values
  are in decimal hours.

- TZ:

  The time zone when the recording started. (default = "local", which
  means user's local time zone)

## Value

A numeric vector of the same length as \`DT\`. Each element is the
elapsed time between the corresponding entry in \`DT\` and the next
boundary (either the next date-time in \`Date\`, or midnight following
the last date), expressed in the units given by \`TUnit\`.

## Examples

``` r
# \donttest{
# Two calendar days: returns c(24, 24) hours
Date2TotalT (as.Date (c ("2021-01-01", "2021-01-02")), "hour")
#> [1] 24 24


# Working in minutes
Date2TotalT (as.POSIXct (c (
    "2021-06-10 08:00:00",
    "2021-06-10 14:30:00"
)), "minute")
#> [1] 390 570
# }

# In seconds (case-insensitive unit name)
Date2TotalT (as.Date ("2022-12-31"), "SeCoNd")
#> [1] 86400
```
