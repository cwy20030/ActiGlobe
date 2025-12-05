# KDE-based circadian cosinor summary

Fit a Gaussian kernel density estimate (KDE) on circular time (hours of
day) weighted by activity and extract first-harmonic cosinor summaries.

## Usage

``` r
CosinorM.KDE(
  time,
  activity,
  bw = 0.8,
  grid = 360L,
  arctan2 = TRUE,
  dilute = FALSE
)
```

## Arguments

- time:

  Numeric vector of time coordinates for each data point. Values are
  interpreted modulo 24 and must lie in \[0, 24). If not numeric, an
  internal function will be used to convert it to numeric values in unit
  of hour.

- activity:

  Numeric vector of activity counts from an actigraphy device in
  correspondance to each time point. Non-numeric inputs will be coerced.
  All-zero or non-finite activity values produce an error.

- bw:

  Numeric scalar. Kernel standard deviation (SD) controlling smoothing.
  bw is interpreted in the same units as the angular scale after
  conversion: the implementation converts \`bw\` from hours to radians
  internally using \\bw\_{rad} = bw \* 2\pi / \tau\\. Default: 0.8.
  Note, small `bw` (approaching 0) can cause numerical instability,
  while large `bw` (approaching 1.5) leads a near-uniform kernel and
  reduces temporal resolution.

- grid:

  Integer number of evaluation points used for the dense grid on \\\[0,
  2 \* \pi)\\. The grid excludes the duplicate 2\*pi endpoint. Default:
  360.

- arctan2:

  Logical; if TRUE (default) acrophase is computed with
  `atan2(gamma, beta)`, resulting in the quadrant interval between
  \\-\pi\\ and \\\pi\\. Whereas, when set to FALSE, the legacy
  arctangent quadrant is mapped. The resulting interval lies between
  \\-\frac{\pi}{2}\\ and \\\frac{\pi}{2}\\.

- dilute:

  Logical; if FALSE (default), all essential parameters would be
  produced. When set to TRUE, only cosinor coefficients are returned.
  This is suited for post-hoc processes, such as computing confidence
  interval via nonparametric bootstrap

## Value

A list of class `c("CosinorM.KDE")` with elements:

- parm: Parameters specified in the model (list with `time`, `activity`,
  `bw`, `grid`)

- tau: Period in hours (default to 24hour)

- kdf: A data.frame with the following columns:

  - theta: Angular positions (radians) at the observation angles

  - density: Estimated PDF values at the observation angles (integrates
    to 1 over \\\[0,2\pi)\\ when scaled)

  - trapizoid.weight: Numeric vector of trapezoid integration weights at
    observation angles

  - kernel.weight: Denominator from kernel convolution (kernel mass at
    each observation angle)

  - fitted.values: Normalized fitted KDE (activity-scale) at observation
    angles

  - fitted.var: Variance estimate of the fitted values (for SE)

  - fitted.se: Standard error of the fitted values

  - hour: Corresponding clock time in hours

- coef.cosinor: Named numeric vector with entries:

  - MESOR: the mean of activity density over 24 hours

  - Amplitude: the first-harmonic amplitude derived from Beta and Gamma

  - Acrophase: Acrophase in radians (time-of-peak) of the dominant daily
    component

  - Acrophase.hr: Acrophase converted to clock hours in \[0,24)

  - Beta: coefficient equivalent to the cosine-weighted integral of the
    KDE

  - Gamma: coefficient equivalent to the sine-weighted integral of the
    KDE

- post.hoc: Post-hoc peak/trough diagnostics derived from fitted.values
  at observation angles (MESOR.ph, Bathyphase.ph.time, Trough.ph,
  Acrophase.ph.time, Peak.ph, Amplitude.ph)

- grid: (when `dilute = FALSE`) list of grid diagnostics:

  - theta: evaluation angles on the dense grid (radians)

  - density: Grid-level PDF (integrates to 1 over theta)

  - trapizoid.weight: Trapezoid integration weights for the grid points

  - kernel.weight: Kernel mass (denominator) at each grid point

  - fitted.values: Normalized fitted values at grid points
    (activity-scale)

  - fitted.var: Variance propagation through fitted variances on the
    grid

  - fitted.se: Standard error of the fitted values at grid points

## Details

This function builds a circular KDE from event times (hours in \\0,24))
and activity. The KDE is computed on a regular angular grid over \\0,
2\*pi) using a wrapped Gaussian kernel with specified bandwidth. Cosinor
summaries (MESOR, Beta, Gamma, Amplitude, Acrophase) are obtained by
numerical integration of the KDE using trapezoid-rule quadrature.

Cosinor parameters are obtained by numerical integration of the KDE on
the grid:

- MESOR: mean level of the fitted curve, \\M = I_0 / (2\pi)\\.

- \\\beta\\: cosine coefficient, \\\beta = I\_{cos} / \pi\\.

- \\\gamma\\: sine coefficient, \\\gamma = I\_{sin} / \pi\\.

- Amplitude: magnitude of the first harmonic, \\A = \sqrt{\beta^2 +
  \gamma^2}\\.

- Acrophase: peak time, \\\phi = atan2(-\gamma, \beta)\\, expressed in
  radians and converted to hours.

The grid integrals are computed with trapezoid weights \\w_g\\: \$\$I_0
= \text{area}\_g \sum_g \frac{pdf_g \\ w_g}{den_g}\$\$ \$\$I\_{cos} =
\text{area}\_g \sum_g \frac{pdf_g \cos(\theta_g)\\ w_g}{den_g}\$\$
\$\$I\_{sin} = \text{area}\_g \sum_g \frac{pdf_g \sin(\theta_g)\\
w_g}{den_g}\$\$

where \\pdf_g\\ is the normalized KDE at grid angle \\\theta_g\\,
\\den_g\\ is the kernel denominator at that grid point, and
\\\text{area}\_g\\ rescales the fitted density back to the activity
scale.

Notes:

- Normalization factors \\1/\pi\\ and \\1/(2\pi)\\ follow from the
  orthogonality of sine and cosine on \\\[0,2\pi)\\.

- The KDE summarizes the first harmonic of the rest–activity pattern
  over one cycle; higher harmonics are not extracted.

- The trapezoid rule ensures numerical stability by integrating over a
  dense, evenly spaced grid.

- The dense grid excludes the duplicate \\2\pi\\ endpoint (grid in
  \\\[0,2\pi)\\) to avoid duplicated quadrature nodes.

- Grid and observation trapezoid weights correct for irregular sampling
  and yield stable approximations to continuous integrals.

- Grid points with near-zero denominator \\\sum_i K(\theta-\theta_i)
  w_i\\ are set to `NA` and excluded from integrals to avoid
  division-by-zero artifacts.

**Residaul Variance and Effective Degree of Freedom**

- **Regression (projection hat matrix):**

  \$\$\hat{\sigma}^2 = \frac{\mathrm{RSS}}{n - p},\$\$

  where \\p\\ is the number of parameters (degrees of freedom used by
  the fit).

- **Linear kernel smoothing:**

  Fitted values are given by the linear smoother

  \$\$\hat{y} = W y,\$\$

  where \\W\\ is the hat matrix induced by the kernel weights (not a
  projection).

When defining residuals as \\r = y - \hat{y}\\ and \\\mathrm{RSS} = \sum
r_i^2\\, the unbiased estimator under the Gaussian kernel smoother can
be written as \$\$\hat{\sigma}^2 = \frac{\mathrm{RSS}}{\\n -
2\\\mathrm{tr}(W) + \mathrm{tr}(W^{\top} W)\\}.\$\$

where,

- \$\$\mathrm{tr}(W)\$\$ is the effective degrees of freedom used by the
  smoother (analogous to the number of parameters).

- \$\$\mathrm{tr}(W^{\top} W) = \sum\_{i,j} W\_{ij}^2\$\$ is a
  correction term because \\W\\ is not an orthogonal projection.

## Warnings and edge cases

- The function errors if `all(activity) == 0` or if `activity` contains
  non-finite values.

- Very small `bw` can cause near-zero denominator values (numerical
  instability) and produce NA or an error; very large `bw` approaches a
  near-uniform kernel and will wash out temporal structure.

## See also

[`CosinorM`](https://cwy20030.github.io/ActiGlobe/reference/CosinorM.md)

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
df <- subset (
    x = df,
    subset = df$Date == "2017-10-27"
)


fit <- CosinorM.KDE (
    time = df$Time,
    activity = df$Activity
)

# inspect coefficients
fit$coef.cosinor

# plot KDE in hours
plot (
    x = fit$kdf$hour,
    y = fit$kdf$density,
    type = "l",
    xlab = "Hour",
    ylab = "KDE"
)
} # }
```
