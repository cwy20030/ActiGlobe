# Plot an Overview of an \`ActiGlobe\` Activity Time Series

Creates a time-series \`scatterplot\` of activity counts from an
\`ActiGlobe\` data.frame,marking each midnight boundary with a vertical
dashed line and \`coloring\` points that were flagged (e.g. travel
overlaps or unallocated epochs).

## Usage

``` r
ggActiGlobe(df, Bdf, VAct = NULL, VDT = "DateTime")
```

## Arguments

- df:

  A data.frame of annotated actigraphy epochs. Must include: - An
  activity column named by \`VAct\`. - A datetime column named by
  \`VDT\`. - Optionally, a \`Note\` column to flag affected epochs. See
  [`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
  and
  [`TAdjust`](https://cwy20030.github.io/ActiGlobe/reference/TAdjust.md)
  for details.

- Bdf:

  A
  [`BriefSum`](https://cwy20030.github.io/ActiGlobe/reference/BriefSum.md)
  object containing per-day metadata for the recording. Note, if jet lag
  occurred during the recording, please, update the metadata using
  [`TAdjust`](https://cwy20030.github.io/ActiGlobe/reference/TAdjust.md)
  before passing to this function.

- VAct:

  Optional character. Name of the activity column in \`df\`. If NULL,
  defaults to the second column of \`df\`.

- VDT:

  Optional character. Name of the POSIXct datetime column in \`df\`. If
  NULL, defaults to "DateTime" of \`df\`.

## Value

A \`ggplot\` object showing: - Activity counts vs. time. - Dashed
vertical lines at each midnight. - Points \`colored\` by whether they
were flagged in \`Note\`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ActiGlobe)

# Overview the Uncorrected Longitudinal Recording
data(FlyEast)

BdfList <-
  BriefSum(
    df = FlyEast,
    SR = 1 / 60,
    Start = "2017-10-24 13:45:00"
  )

p <- ggActiGlobe(
  df = BdfList$df,
  Bdf = BdfList$Bdf,
  VAct = "Activity",
  VDT = "DateTime"
)

print(p)


# Overview the Corrected Longitudinal Recording
data(TLog)

BdfList$Bdf.adj <- TAdjust(BdfList$Bdf, TLog)
p2 <- ggActiGlobe(
  df = BdfList$df,
  Bdf = BdfList$Bdf,
  VAct = "Activity",
  VDT = "DateTime"
)
print(p2)

# Pro-tip: [`cowplot`] can help stack the time series graphs in one
# single plot
} # }
```
