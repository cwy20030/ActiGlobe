# Bootstrap Standard Errors and Confidence Intervals for Model Parameters

Computes standard errors and confidence intervals for cosinor and
post-hoc parameters via non-parametric bootstrap

## Usage

``` r
boot.seci(object, level = 0.95, N = 500, digits = 2)
```

## Arguments

- object:

  A fitted \`CosinorM\` or \`CosinorM.KDE\` model object.

- level:

  Numeric scaler. the confidence level.

- N:

  Numeric scaler. Numbers of bootstraps required to estimate the
  standard errors and confidence intervals. Default: 500

- digits:

  Numeric scaler. Integer indicating the number of decimal places
  (round) to be used. Default: 2

## Value

A data.frame with one row per cosinor coefficient and columns:

- Estimate: Mean of bootstrap coefficient values.

- Std Error: Bootstrap standard deviation of each coefficient across N
  resamples.

- t value: Ratio of the observed estimate to its bootstrap standard
  error, analogous to a signal-to-noise measure: \$\$t =
  \hat{\theta}\_obs / SE_boot\$\$

- lower CI label: Percentile lower bound at \\\frac{\alpha}{2}\\, where
  \\\alpha = 1 - level\\.

- upper CI label: Percentile upper bound at \\1 - \frac{\alpha}{2}\\.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Import data
FlyEast

BdfList <-
    BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

# Let's extract actigraphy data from a single day
df <- BdfList$df
df <- subset (df, df$Date == "2017-10-27")

# Multicomponent Cosinor Model
fit <- CosinorM (
    time = df$Time,
    activity = df$Activity,
    tau = c (12, 24),
    method = "OLS"
)

# inspect coefficients

boot.seci (
    object = fit,
    level = 0.95,
    N = 500
)


# Gaussian Kernel Density Estimation
fit2 <- CosinorM.KDE (
    time = df$Time,
    activity = df$Activity
)

# inspect coefficients

boot.seci (
    object = fit2,
    level = 0.95,
    N = 500
)
} # }
```
