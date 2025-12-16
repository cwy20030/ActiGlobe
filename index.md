# ActiGlobe

ActiGlobe is designed to make it easy to pre-process longitudinal
actigraphy recordings, especially for the recordings affected by time
shift due to daylight saving changes and or cross-continental travel. It
streamlines the process from end-to-end, to simplify the process of
pre-processing, analyzing, and exporting daily actigraphy data and
visual reports, making it an essential tool for researchers and
professionals in the field.

## Installation

The pre-released version of ActiGlobe can installed from
[GitHub](https://github.com/) with:

#### ActiGlobe-dev

``` r
# If devtools is not available locally, please download it by removing the number symbol before the 'install.packages' code. 
# install.packages("devtools")
devtools::install_github("cwy20030/ActiGlobe")


# To properly install tutorial, please, use the following code
devtools::install_github("cwy20030/ActiGlobe",
                         build_vignettes = TRUE)
```

Coming soon… ActiGlobe-release

``` r
install.packages("ActiGlobe")
```

  

## Citation

### For the pre-release package ‘ActiGlobe’ in publication:

``` r
citation("ActiGlobe")
#> To cite package 'ActiGlobe' in publications use:
#> 
#>   Yao C, Varesco G, Simonelli G (2025). _ActiGlobe: Wearable Recording
#>   Processor for Cross-Continental Travel_. R package version 0.1.9,
#>   <https://github.com/cwy20030/ActiGlobe>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ActiGlobe: Wearable Recording Processor for Cross-Continental Travel},
#>     author = {C. William Yao and Giorgio Varesco and Guido Simonelli},
#>     year = {2025},
#>     note = {R package version 0.1.9},
#>     url = {https://github.com/cwy20030/ActiGlobe},
#>   }
```

To convert to an EndNote-compatible format, paste the BibTeX entry in
\[online bibtex-converter\]
(<https://asouqi.github.io/bibtex-converter/>)

## Quick Start - No Time Change

``` r
### Load ActiGlobe into R
library(ActiGlobe)
```

``` r
### Replace FlyEast with the dataset and specify sampling rate in SR and the start of the recording. 
BdfList = 
BriefSum(df = FlyEast,
         SR = 1/60,
         Start = "2017-10-24 13:45:00")


### Extract the summary report and the enriched data
Bdf <- BdfList$Bdf
df <- BdfList$df
```

``` r
### Quick overview of the original recording
ggActiGlobe(df = df, 
            Bdf = Bdf,
            VAct = "Activity",
            VDT = "DateTime")
```

![Figure 1. An Overview of the Raw
Recording](/articles/images/Unadjusted_Overview.png) **Figure 1.**
Overview of the raw recording with clear day‑to‑day epoching
misalignment

### Adjust Travel-induced Time Shift

``` r
#### Import the travel log into R and give it a name
TLog <- read.csv("WHERE/YOU/STOREd/THE/TRAVEL/LOG/TEMPLATE/TravelLog_Template.csv")

#### Replace the TLog with the name of the travel log assigned
Bdf.adj = TAdjust(Bdf = Bdf, 
                  TLog = TLog)
```

Take a coffee break if needed because ActiGlobe will adjust time shift
and anonymize the travel destination to keep participants’ privacy.

``` r
dfList = Act2Daily(df = df,
                   Bdf = Bdf.adj,
                   VAct = "Activity",
                   VTm = "Time",
                   Incomplete = TRUE,
                   Travel = TRUE)
```

#### Review adjustment

``` r
df2 <- do.call(rbind, dfList$Daily_df)

ggActiGlobe(df = df2, 
            Bdf = Bdf.adj,
            VAct = "Activity",
            VDT = "DateTime")
```

![Figure 2. An Overview of the Adjusted
Recording](/articles/images/Adjusted_Overview.png) **Figure 2.**
Overview of the *adjusted* recording

## Other Features

Generate report via write.cosinor() and export pre-processed data via
write.act() reproducibility and further analysis.

ActiGlobe can also be used to analyze data via traditional OLS cosinor
modeling. To learn how to perform and visualize ecnometrics-based
cosinor model and circularized kernel density estimation, please, see
the package vignettes.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/cwy20030/ActiGlobe/blob/main/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
