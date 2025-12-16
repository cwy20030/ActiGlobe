# Detect Possible Time Format

This function will automatically detect possible time format. Users can
choose to either reformat the date or report the datetime format
detected.

## Usage

``` r
TimeFormat(time, as.time = FALSE)
```

## Arguments

- time:

  A character string of time.

- as.time:

  A binary operator indicating whether to return a converted time based
  on the detection or the time format. (default: FALSE, which returns
  time format)

## Value

If `as.time = TRUE`, returns a character vector of the input times
reformatted according to the detected format. Each element corresponds
to the respective entry in `time`.

If `as.time = FALSE`, returns a character scalar giving the
best-matching time format string.

If no format matches, returns NA and issues a warning.

## Examples

``` r
# Example 1: When all dates have the same format
## Create and store a date in a variable called Time
Time <- c (
    "2017/05/02 23:00:01", "1970/01/02 05:10:33",
    "2000/02/28 07:00:00"
)

## Ask TimeFormat to tell us the format of the time.
TimeFormat (time = Time, as.time = FALSE)
#> [1] "%H:%M:%S"

# Example 2: When multiple formats co-exist in a variable
## Create and store dates and time in a variable called Time
Time <- c (
    "2017/05/02 23:00:01", "2000/02/28 07:00", "1970/01/02",
    "2022/11/28 08:35 PM"
)

## Ask TimeFormat to tell us the format of the Time.
### DO This!
#### Option 1.
print (lapply (Time, TimeFormat))
#> Warning: No time format matched! Please, specify the time format.
#> [[1]]
#> [1] "%H:%M:%S"
#> 
#> [[2]]
#> [1] "%H:%M"
#> 
#> [[3]]
#> [1] NA
#> 
#> [[4]]
#> [1] "%I:%M %p"
#> 

#### Option 2. To match the warning message to the items
for (x in Time) {
    print (TimeFormat (x))
}
#> [1] "%H:%M:%S"
#> [1] "%H:%M"
#> Warning: No time format matched! Please, specify the time format.
#> [1] NA
#> [1] "%I:%M %p"


### DO NOT!
### print(TimeFormat(time = Time))
### Note, this process will fail because there are multiple formats
```
