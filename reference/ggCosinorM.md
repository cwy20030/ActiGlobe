# Plot \`CosinorM\` and \`CosinorM.KDE\` Fit with \`ggplot2\`

Create visualization of a CosinorM or CosinorM.KDE fit using ggplot2.
The plot shows the parametric cosinor fit over a fine time grid,
optional pointwise confidence bands, observed data points, MESOR line,
acrophase verticals, amplitude annotation segments, and labelled
parameter values when requested.

## Usage

``` r
ggCosinorM(
  object,
  labels = TRUE,
  ci = TRUE,
  ci_level = 0.95,
  n = 400,
  point_size = 0.5,
  title_extra = NULL,
  legend.position = "right",
  ...
)
```

## Arguments

- object:

  A fitted model of class
  [`CosinorM`](https://cwy20030.github.io/ActiGlobe/reference/CosinorM.md)
  or
  [`CosinorM.KDE`](https://cwy20030.github.io/ActiGlobe/reference/CosinorM.KDE.md)

- labels:

  Logical; Default \`TRUE\` places repelled labels on the plot with
  MESOR, amplitude(s), and acrophase(s).

- ci:

  Logical; Default \`TRUE\` computes and draws pointwise parametric
  confidence bands for the fitted cosinor curve using the model
  covariance.

- ci_level:

  Confidence level for the pointwise bands, expressed in numeric value
  between 0 and 1.

- n:

  Integer; number of points on the fine prediction grid used to draw the
  fitted cosinor and confidence ribbon. (default to \`400\`)

- point_size:

  Numeric; plotting size for observed points. (default: \`0.5\`)

- title_extra:

  Optional character string appended to the plot title for extra
  context.

- legend.position:

  Position of the legend on the plot; default is \`"right"\`. Other
  options include \`"top"\`, \`"bottom"\`, \`"left"\`, or a numeric
  vector of length two specifying x and y coordinates.

- ...:

  Additional arguments (currently ignored) kept for future update

## Value

A \`ggplot\` object representing the cosinor model fit visualization.

## See also

[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html),
[`geom_label_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
[`predict`](https://rdrr.io/r/stats/predict.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Import data
data (FlyEast)

BdfList <-
    BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

# Let's extract actigraphy data from a single day
df <- BdfList$df
df <- subset (df, df$Date == "2017-10-28")

fit <- CosinorM (
    time = df$Time,
    activity = df$Activity,
    tau = 24,
    method = "OLS"
)


p <- ggCosinorM (
    object = fit,
    labels = TRUE,
    ci = TRUE,
    ci_level = 0.95,
    title_extra = "2017-10-24"
)
print (p)
} # }
```
