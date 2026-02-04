# Radian to Hour

Convert \`acrophase\` in radian to clock time based on tau.

## Usage

``` r
Rad2Hr(x, tau)
```

## Arguments

- x:

  Numeric vector of phases in radian.

- tau:

  Numeric scalar. Period length in the same time units you want returned
  (for hours use 24). Must be a single numeric value \> 0 and \<= 24.

## Value

Numeric vector of times in \\\[0, tau)\\. NA values propagate.

## Examples

``` r
Rad2Hr (pi / 2, tau = 24)
#> [1] 6

Rad2Hr (c (-pi / 2, 0, pi, 3 * pi / 2), tau = 24)
#> [1] 18  0 12 18
```
