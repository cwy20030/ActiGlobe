# Compute UTC offset based on Time Zone and Date

Compute UTC offset based on Time Zone and Date

## Usage

``` r
TZ2UTC(DT, TZ = "local")
```

## Arguments

- DT:

  The date in the format as "2021-03-05"

- TZ:

  The time zone when the recording started. (default = "local", which
  means user's local time zone)

## Value

A character string indicating the UTC offset, e.g., "UTC-05:00"

## Examples

``` r
x <- as.Date(c("2017-10-24", "2017-11-20"))
TZ2UTC(DT = x, TZ = "America/New_York") ## Expect two different UTCs
#> [1] "UTC-04:00" "UTC-05:00"

if (FALSE) { # \dontrun{
# A vector of dates
x <-
  seq.Date(
    from = as.Date("2017-10-24"),
    to = as.Date("2017-11-27"),
    by = "day"
  )

# If the user resides in a country that follows daylight saving time
TZ2UTC(DT = x, TZ = "local")

# If not
# TZ2UTC(DT = x, TZ = "America/New_York")
} # }
```
