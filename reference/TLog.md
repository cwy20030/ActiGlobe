# Travel Log

A modified travel log documenting the dates when the wearer departs for
long-distance travel.

## Usage

``` r
data(TLog)
```

## Format

A data frame with over 40,000 rows and 5 variables:

- ID:

  Pseudonym of the wearer.

- UTC_Offset:

  UTC offset of the wearer's location or destination.

- Country_with_Daylight_Saving:

  Binary indicator (TRUE/FALSE) for whether daylight saving time is
  observed at the location or destination.

- date_Start:

  Start date of the wearer's initial location or date of departure.

- date_End:

  Optional date when the wearer departs again from the initial location
  or previous destination.

## See also

[`TAdjust`](https://cwy20030.github.io/ActiGlobe/reference/TAdjust.md)
[`TravelLog`](https://cwy20030.github.io/ActiGlobe/reference/TravelLog.md)
[`IANA`](https://cwy20030.github.io/ActiGlobe/reference/IANA.md)
