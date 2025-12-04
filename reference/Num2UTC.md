# Convert Numbers to Standard UTC Offset

Convert numeric values representing UTC offsets (in hours, with optional
fractional parts for minutes) into standardized character strings of the
form \`"UTC+HH:MM"\` or \`"UTC-HH:MM"\`. This is useful for translating
numeric time zone offsets into human-readable UTC offset notation.

## Usage

``` r
Num2UTC(x)
```

## Arguments

- x:

  UTC Offsets \<e.g., "UTC+09:30" or "UTC-07:00"\>

## Value

A character vector of standard UTC offsets

## See also

[`UTC2Num`](https://cwy20030.github.io/ActiGlobe/reference/UTC2Num.md)

## Examples

``` r
# Convert UTC to numeric values
x <- c (9.5, -7)

x1 <- Num2UTC (x)

print (x1)
#> [1] "UTC+09:30" "UTC-07:00"
```
