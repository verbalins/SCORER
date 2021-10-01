
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SCORER

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of SCORER is to allow for

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("verbalins/SCORER")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SCORER)

# Launch the knowledge extraction browser
SCORER::run_app()

# To pre-populate the browser with a dataset, 
# use the dataset parameter with data imported by
# the load_dataset function.
# df <- SCORER::load_dataset("datafile.csv")
# SCORER::run_app(dataset=df)
```
