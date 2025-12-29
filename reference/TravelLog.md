# Generate the Travel Log Template Needed for Time Zone Correction

Generate the Travel Log Template Needed for Time Zone Correction

## Usage

``` r
TravelLog(Write = FALSE, Dir = NULL)
```

## Arguments

- Write:

  A binary code to indicate whether to write a .csv file containing the
  template needed for the travel log. (default = FLASE) When set to
  FALSE, the template will be returned as an object. When set to TRUE,
  user must provide the target directory where they wish to store the
  template.

- Dir:

  The directory where the travel log template to be exported \<e.g.
  "C:/Users/\_\_\_YOUR USERNAME\_\_\_/UPSTREAM FOLDER/.../FOLDER
  NAME/"\>

## Value

a travel-log template as a data.frame or written as a CSV file

## Examples

``` r
Tlg <- TravelLog (Write = FALSE)

print (Tlg)
#>          ID UTC_Offset Country_with_Daylight_Saving date_Start   date_End
#> 1 ExampleID     +05:00                         TRUE 2025-12-29 2025-12-30
```
