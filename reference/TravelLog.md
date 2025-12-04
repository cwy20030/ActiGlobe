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
# example code
if (FALSE) { # \dontrun{

Tlg <- TravelLog(Wirte = TRUE)

print(Tlg)

} # }
```
