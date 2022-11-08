
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SCORER

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/299561085.svg)](https://zenodo.org/badge/latestdoi/299561085)
<!-- badges: end -->

The goal of SCORER is to allow reproducible knowledge extraction of
multi-objective optimization data, primarily obtained from
simulation-based optimization. The package contains methods as well as a
Shiny application reachable at
[shinyapps.io](https://verbalins.shinyapps.io/SCORER/) if you want to
preview the app.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("verbalins/SCORER", dependencies = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SCORER)

# Launch the knowledge extraction browser
SCORER::run_app()

# To pre-populate the browser with a dataset, 
# use the dataset parameter with data imported by
# the loaddataset function.
# df <- SCORER::loaddataset("datafile.csv")
# SCORER::run_app(dataset=df)
```
