
# ratPASTA <img src='man/figures/logo.png' align="right" height="120" />

<!-- README.md is generated from README.Rmd. Please edit that file -->

# R-based Awesome Toolbox for Platform for Acoustic STArtle

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/ikodvanj/ratPASTA.svg?branch=master)](https://travis-ci.com/ikodvanj/ratPASTA)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ikodvanj/ratPASTA?branch=master&svg=true)](https://ci.appveyor.com/project/ikodvanj/ratPASTA)
[![Codecov test
coverage](https://codecov.io/gh/ikodvanj/ratPASTA/branch/master/graph/badge.svg)](https://codecov.io/gh/ikodvanj/ratPASTA?branch=master)
<!-- badges: end -->

ratPASTA is a package used for processing and visualising data from
startle experiments in rodents. The input data for this package is
created with a PASTA solution (Platform for Acoustic STArtle), described
in detail here:

Virag, D., Homolak, J., Kodvanj, I., Babic Perhoc, A., Knezovic, A.,
Osmanovic Barilar, J., & Salkovic-Petrisic, M. (2020). Repurposing a
digital kitchen scale for neuroscience research: a complete hardware and
software cookbook for PASTA. BioRxiv, 2020.04.10.035766.
<https://doi.org/10.1101/2020.04.10.035766>

[Link to
preprint](https://www.biorxiv.org/content/10.1101/2020.04.10.035766v1)

## Installation

Install ratPASTA from Github:

``` r
install_github("ikodvanj/ratPASTA")
```

## Features

Package includes the following functions:

  - `loadStartleData()` - loads and merges all pasta files (startle
    data)
  - `basicStartlePlot()` - creates a time-series plot
  - `startlePlot()` - returns several plots
  - `summariseStartle()` - returns a mathematical summary of the data
  - `latencyPlot()` - creates a list with two latency plots

## Instructions

For instructions on how to use this package read the
[vignette](https://ikodvanj.github.io/ratPASTAsite/articles/ratPASTA.html)
