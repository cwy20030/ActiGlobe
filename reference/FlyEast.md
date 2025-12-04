# Travel East Sample Data

This sample data is a modified version of a continuous wrist-worn
actigraphy recording (Philips \`ActiWatch5\`) collected over a month.
The data was originally collected to monitor the influence of jetleg on
professional athelets' performance during competitions.

## Usage

``` r
data(FlyEast)
```

## Format

A modified data with 40 thousands plus rows and 3 variables:

- Activity:

  The activity count measured by a tri-axis actigraphy

- X2:

  The light exposure data from the light sensor. All values are zero
  since the function was not disabled during the recording.

- Marker:

  A binary indicator generated when the wearer press the button to
  record onset of sleep or awake.

## See also

[`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
[`TAdjust`](https://cwy20030.github.io/ActiGlobe/reference/TAdjust.md)
[`TravelLog`](https://cwy20030.github.io/ActiGlobe/reference/TravelLog.md)
[`IANA`](https://cwy20030.github.io/ActiGlobe/reference/IANA.md)
