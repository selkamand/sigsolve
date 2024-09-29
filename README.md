
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigfit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sigfit)](https://CRAN.R-project.org/package=sigfit)
[![R-CMD-check](https://github.com/selkamand/sigfit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/sigfit/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/sigfit/branch/main/graph/badge.svg)](https://app.codecov.io/gh/selkamand/sigfit?branch=main)
![GitHub Issues or Pull
Requests](https://img.shields.io/github/issues-closed/selkamand/sigfit)
[![code
size](https://img.shields.io/github/languages/code-size/selkamand/sigfit.svg)](https://github.com/selkamand/sigfit)
![GitHub last
commit](https://img.shields.io/github/last-commit/selkamand/sigfit)
<!-- badges: end -->

> \[!WARNING\]  
> This package is in early development and not yet ready for use

The **sigfit** R package implements common mutational signature fitting
algorithms that allow you to identify the most likely combination of
known mutational signatures which explain the mutational profile
observed in a sample of interest.

> \[!TIP\]  
> Sigfit is exclusively a signature fitting tool. For turnkey end-to-end
> signature analysis we recommend
> [sigscreen](https://github.com/selkamand/sigscreen)

Unlike most other implementations, sigfit expects no specific signature
collection or feature space. Users supply a catalogue of counts & a
matching signature collection, so any type of signature analysis /
feature space is supported.

## Installation

You can install the development version of sigfit like so:

``` r
if (!require("remotes"))
    install.packages("remotes")

remotes::install_github("selkamand/sigfit")
```

## Quick Start

There are 2 inputs required.

1.  **observed mutation counts**: A named numeric vector where names
    represent channels and values represent counts
2.  **signature collection**: A channel X signature matrix where values
    represent fractions.

``` r
library(sigfit)

catalogue <- c(channel1 = 20, channel2 = 104, channel3 = 10)

signatures <- matrix(
  c(0, 0.9, 0.1, 0.98, 0.01, 0.01, 0.25, 0.5, 0.25),
  nrow = 3L,
  ncol = 3L,
  dimnames = list(c("channel1", "channel2", "channel3"), c("Sig1", "Sig2", "Sig3"))
)

sig_fit(catalogue, signatures, method = "nnls")
```
