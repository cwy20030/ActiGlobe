# Detect Possible DT Format

This function will automatically detect possible date format. Users can
choose to either reformat the date or report the datetime format
detected.

Identifies the likely date format used in character strings and
optionally converts these strings to Date objects. This utility is
intended to support pre-processing of timestamp data with varied or
unknown formatting. When ambiguity arises (e.g., day-month versus
month-day ordering), the function defaults to the first compatible
format unless explicitly guided by a delimiter or by manual correction.

## Usage

``` r
DateFormat(DT, as.date = TRUE, Delim = NULL)
```

## Arguments

- DT:

  A character vector containing date or date-time strings.

- as.date:

  Logical. If TRUE (default), returns converted Date objects. If FALSE,
  returns the detected format string (e.g., `"%d/%m/%Y"`).

- Delim:

  Optional. A single character (e.g., `"."`, `"-"`, or `"/"`) to
  override default date delimiters. Requited when input strings may
  contain nonstandard separators (e.g., `"$"`, `"~"`).

## Value

If `as.date = TRUE`, returns a vector of class `Date`.

If `as.date = FALSE`, returns a character string representing the
detected format. When multiple formats coexist or no valid format is
detected, warnings are issued and default behavior is applied.

If no format matches, returns `NA` and issues a warning.

## Examples

``` r
# Consistent format across all strings
DT <- c ("2017/05/02", "2000/02/28", "1970/01/02")
DateFormat (DT, as.date = FALSE) # returns parsed Date vector
#> [1] "%Y/%m/%d"

if (FALSE) { # \dontrun{
# Mixed formats within a vector
DT <- c (
    "2017/05/02", "2000.Feb.28", "1970-11-02",
    "January 01, 2025", "December 12, 1980"
)
lapply (DT, DateFormat) # element-wise parsing
### We expect that many of these format will not work because they contain
### text


DT <- c (
    "2017/05/02", "2000.02.28", "1970-11-02",
    "01, 01, 2025", "12, 12, 1980"
)

# Recommended usage for mixed formats:
lapply (DT, DateFormat, Delim = ",") # element-wise parsing

for (x in DT) {
    print (DateFormat (x, Delim = ","))
} # displays format/warning per entry

# Avoid using sapply, because it will convert them into numeric form
sapply (DT, DateFormat)
} # }
```
