# Convert Standard UTC Offset to Numbers

Convert Standard UTC Offset to Numbers

## Usage

``` r
UTC2Num(x)
```

## Arguments

- x:

  Character string of UTC offsets, e.g., "UTC+09:30" or "UTC-07:00".

## Value

Numeric values of UTC offsets

## Examples

``` r
# Convert UTC to numeric values
x <- c ("UTC+09:30", "UTC-07:00")

x1 <- UTC2Num (x)

print (x)
#> [1] "UTC+09:30" "UTC-07:00"
```
