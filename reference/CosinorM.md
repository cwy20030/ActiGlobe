# \`Cosinor\` Model

Fit a cosine-based harmonic linear regression on circular time (hours of
day)

## Usage

``` r
CosinorM(
  time,
  activity,
  tau,
  method = "OLS",
  arctan2 = TRUE,
  type = "HC3",
  dilute = FALSE
)
```

## Arguments

- time:

  Numeric vector of time coordinates for each data point

- activity:

  Numeric vector of activity counts from an actigraphy device

- tau:

  Numeric scalar or vector for the assumed circadian period. Default is
  24 for single-phase; multiple phases can be supplied via \[c()\]

- method:

  Character string specifying estimation method

  - "OLS": Ordinary least squares via
    [`lm`](https://rdrr.io/r/stats/lm.html) (default)

  - "FGLS": Feasible generalized least squares; models
    heteroskedasticity via a log-variance fit to squared OLS residuals,
    computes weights, and refits by weighted least squares

- arctan2:

  Logical; if TRUE (default) acrophase is computed with
  `atan2(gamma, beta)`, resulting in the quadrant interval between
  \\-\pi\\ and \\\pi\\. Whereas, when set to FALSE, the legacy
  arctangent quadrant is mapped. The resulting interval lies between
  \\-\frac{\pi}{2}\\ and \\\frac{\pi}{2}\\.

- type:

  Character string passed to
  [`vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  for robust standard error computation

- dilute:

  Logical;

  - "FALSE": All essential parameters would be produced. (default)

  - "TRUE": Only cosinor coefficients are returned. This is suited for
    post-hoc processes, such as computing confidence interval via
    nonparametric bootstrap

## Value

A list of class c("CosinorM", "lm") containing:

- parm: Parameters specified in the model

- tau: The assumed period length

- time: The time coordinates of the recording

- method: The estimation method used

- coef.cosinor: Named numeric vector with entries:

  - MESOR: Mid-line estimating statistic of rhythm

  - Amplitude: Predicted peak activity magnitude

  - Acrophase: Acrophase in radian (time-of-peak)

  - Beta: the coefficient of the cosine function

  - Gamma: the coefficient of the sine function

- post.hoc: Post-hoc peak/trough diagnostics derived from fitted.values
  at observation angles (MESOR.ph, Bathyphase.ph.time, Trough.ph,
  Acrophase.ph.time, Peak.ph, Amplitude.ph)

- extra: only available for ultradian (i.e., \\\tau\\ less than 24hour)
  single-component model

- vcov: Robust variance-covariance matrix

- se: Standard errors

Inherits all components from [`lm`](https://rdrr.io/r/stats/lm.html).

## Details

The \`Cosinor\` model is a cosine-based harmonic regression used to
estimate circadian rhythm parameters.

**Single-phase equation**: \$\$y = M + A \cos \left(\frac{2\pi
t}{\tau} - \phi\right) \$\$

- \\M \\: MESOR (mid-line estimating statistic of rhythm), the intercept

- \\A \\: Amplitude, peak deviation from M

- \\t \\: Time coordinate within the cycle

- \\\tau \\: The assumed period length

- \\\phi \\: Acrophase (time-of-peak), computed from fitted sine and
  cosine coefficients

Note that because the model is parameterized with a negative phase
inside the cosine, this means the the derived peak time from acrophase
corresponds to the peak time forward from midnight (not backward). In
other words, the when the acrophase is positive, it means the peak
activity time occurs after midnight and vice versa.

**Linearized form**: \$\$\hat{y} = M + \beta x + \gamma z + \epsilon
\$\$

- \\\beta = A \* cos(\phi)\\, estimated coefficient for the cosine term

- \\x = cos(\frac{2 \* \pi \* t} {\tau})\\, cosine-transformed time

- \\\gamma = A \* sin(\phi)\\, estimated coefficient for the sine term

- \\z = sin(\frac{2 \* \pi \* t} {\tau})\\, sine-transformed time

- \\\epsilon \\: error term

Model parameters are estimated by minimizing the residual sum of
squares: \$\$RSS = \sum\_{i=1}^n (y_i - (M + \beta x_i + \gamma
z_i))^2\$\$ By default, [`lm`](https://rdrr.io/r/stats/lm.html) fits
this via QR decomposition.

**Acrophase \\\phi\\ interpretation**: \\\phi \\ is derived from
\$\$\phi = \arctan2(\frac{\gamma} {\beta}) \$\$

Note,

- \\\phi \\ can be converted to clock time to identify the peak activity
  time AFTER midnight.

- if the model is not a mono-phase (i.e., more than one \\\tau \\),
  there will be \\\frac{24 (hours)} {\tau} \\ number of peaks within a
  day.

**Amplitude \\(A) \\ estimation**: Amplitude is calculated from fitted
sine and cosine coefficients as: \$\$A= \sqrt {(\beta^2 + \gamma^2)}\$\$

## References

Cornelissen G. Cosinor-based rhythmometry. Theoretical Biology and
Medical Modelling. 2014-12-01 2014;11(1):16. doi:10.1186/1742-4682-11-16

Chambers, J. M. (1992) Linear models. Chapter 4 of Statistical Models in
S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

Wilkinson, G. N. and Rogers, C. E. (1973). Symbolic descriptions of
factorial models for analysis of variance. Applied Statistics, 22,
392-399. doi:10.2307/2346786.

Harvey, A. C. (1976). Estimating Regression Models with Multiplicative
Heteroscedasticity. Econometrica, 44(3), 461-465. doi:10.2307/1913974

## See also

[`lm`](https://rdrr.io/r/stats/lm.html)
[`CosinorM.KDE`](https://cwy20030.github.io/ActiGlobe/reference/CosinorM.KDE.md)

## Examples

``` r
require (stats)
require (graphics)

if (FALSE) { # \dontrun{
# Import data
data (FlyEast)


# Create quick summary of the recording with adjustment for daylight saving.
BdfList <-
    BriefSum (
        df = FlyEast,
        SR = 1 / 60,
        Start = "2017-10-24 13:45:00"
    )

# Let's extract actigraphy data from a single day
df <- BdfList$df
df <- subset (df, df$Date == "2017-10-27")

fit <- CosinorM (
    time = df$Time,
    activity = df$Activity,
    tau = 24,
    method = "OLS"
)


# inspect coefficients
fit$coef.cosinor


# plot Cosinor in hours
plot (fit$time, fit$fitted.values, type = "l", xlab = "Hour",
      ylab = "24-Hour Cosinor Model")
} # }
```
