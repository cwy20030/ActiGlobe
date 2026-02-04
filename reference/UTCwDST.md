# Determine if Daylight Saving Transitions may occur in an UTC Offset

Tests whether any IANA time zone associated with a given UTC offset
undergoes daylight saving time (DST) transitions. The function
identifies all zones matching the specified UTC offset and evaluates
whether a time change occurs between winter and summer timestamps.

## Usage

``` r
UTCwDST(UTCs, DT = NULL, fork = FALSE)
```

## Arguments

- UTCs:

  A character vector or numeric vector representing UTC offsets.
  Accepted formats include "UTC+08:00", "UTC-05:00", or numeric values
  like "+8", "-5", etc. The function internally maps UTC strings to
  numeric offsets using
  [`UTC2Num`](https://cwy20030.github.io/ActiGlobe/reference/UTC2Num.md).

- DT:

  A POSIXct date/time used as the reference point for offset comparison.
  Defaults to January 1, 2021 UTC if \`NULL\`.

- fork:

  Logical, if TRUE, it will use parallel processing to speed up the
  computation. Default is FALSE.

## Value

A logical vector the same length as `UTCs`. Each entry is \`TRUE\` if at
least one time zone at the specified offset undergoes a DST transition,
\`FALSE\` otherwise.

## Details

For each value in `UTCs`, the function retrieves the corresponding IANA
time zones whose offset matches the specified UTC string or numeric
hour. It then compares the DST status of each zone on January 1 and July
15. If at least one zone shows a shift in DST status between the two
dates, the function returns \`TRUE\` for that offset.

## See also

[`DST`](https://cwy20030.github.io/ActiGlobe/reference/DST.md)
[`UTC2Num`](https://cwy20030.github.io/ActiGlobe/reference/UTC2Num.md)
[`OlsonNames`](https://rdrr.io/r/base/timezones.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check for DST transitions in UTC+1 and UTC+8
UTCwDST (UTCs = c ("UTC+01:00", "UTC+08:00"))

# Use numeric offset directly
UTCwDST (UTCs = c (1, 8))

# UTC-5 commonly includes DST zones (e.g., New York)
UTCwDST (UTCs = -5)
} # }
```
