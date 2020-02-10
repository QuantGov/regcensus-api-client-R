
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regcensusAPI

<!-- badges: start -->

<!-- badges: end -->

The goal of regcensusAPI is to provide an easy way for researchers to
access regulatory restrictions data from the Mercatus Center at George
Mason University, RegData. RegData seeks to quantify the volume of
regulatory restrictions in various jurisdictions. See more about RegData
from the [QuantGov](https://www.quantgov.org) website.

## Installation

You can install the released version of regcensusAPI from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("regcensusAPI")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("QuantGov/regcensus-api-client-R")
```

## Usage

Using the API is easy. First, you would want to find out what topics are
covered.

``` r
library(regcensusAPI)
#> Welcome to RegCensus API - Pulling initial data
#> Registered S3 method overwritten by 'R.oo':
#>   method        from       
#>   throw.default R.methodsS3
#> API call: https://api.quantgov.org/periods/available
#> API call to https://api.quantgov.org/periods/available complete.
#> API call: https://api.quantgov.org/industries?jurisdiction=38
#> API call to https://api.quantgov.org/industries?jurisdiction=38 complete.
## basic example code
get_topics()
#> API call: https://api.quantgov.org/topics/NA
#> URL is not correctly formed: https://api.quantgov.org/topics/NA
#> Original error message:
#> API call to https://api.quantgov.org/topics/NA complete.
```
