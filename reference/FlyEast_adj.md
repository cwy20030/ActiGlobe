# Adjusted Travel East Sample Data

This is the subset of the travel-adjusted \`FlyEast\` data. The data was
originally collected to monitor the influence of jetleg on professional
athelets' performance during competitions.

## Usage

``` r
data(FlyEast_adj)
```

## Format

An adjusted FlyEast data with 40 thousands plus rows and 6 variables:

- DateTime:

  The time coordinates of each recorded activies in the standard date
  and time joint format

- Date:

  The date of each recorded activies

- Hour:

  The simplified time coordinates in hour form converted from
  \`DateTime\`

- Activity:

  The activity count measured by a tri-axis actigraphy

- Activity_ID:

  Numeric values denoted the sequence of each activity count in the
  recording

- Note:

  Summary annotation derived from
  [`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md).

## See also

[`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
[`TAdjust`](https://cwy20030.github.io/ActiGlobe/reference/TAdjust.md)
[`FlyEast`](https://cwy20030.github.io/ActiGlobe/reference/FlyEast.md)
