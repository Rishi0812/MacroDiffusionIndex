
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MacroDiffusionIndex

<!-- badges: start -->

![R-CMD-check](https://github.com/Rishi0812/MacroDiffusionIndex/actions/workflows/slack-notify-build.yml/badge.svg)
<!-- badges: end -->

A R Package to apply Machine Learning to Macro Economic Diffusion
Indexes

## Installation

You can install the development version of MacroDiffusionIndex from
[GitHub](https://github.com/Rishi0812/MacroDiffusionIndex) with:

``` r
# install.packages("devtools")
devtools::install_github("Rishi0812/MacroDiffusionIndex")
```

Make sure you have the devtools package installed and called to use the
above command.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MacroDiffusionIndex)
#> Loading required package: quantmod
#> Loading required package: xts
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: TTR
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
mdi_get_data() #Dataset will be stored in Global Environment
#> [1] "WLEMUINDXD"
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
