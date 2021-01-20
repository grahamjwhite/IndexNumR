
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IndexNumR

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/grahamjwhite/IndexNumR.svg?branch=master)](https://travis-ci.org/grahamjwhite/IndexNumR)

[![Codecov test
coverage](https://codecov.io/gh/grahamjwhite/IndexNumR/branch/master/graph/badge.svg)](https://codecov.io/gh/grahamjwhite/IndexNumR?branch=master)

[![Rdoc](http://www.rdocumentation.org/badges/version/IndexNumR)](http://www.rdocumentation.org/packages/IndexNumR)

[![CRAN](https://www.r-pkg.org/badges/version/IndexNumR)](https://www.r-pkg.org/badges/version/IndexNumR)
<!-- badges: end -->

An R package for computation of index numbers

IndexNumR provides a set of functions for computing various bilateral
and multilateral indices. It is designed to compute price or quantity
indices over time. Bilateral indices include Laspeyres, Paasche, Fisher,
Tornqvist, Sato-Vartia, Walsh, CES, geometric Laspeyres, geometric
Paasche, time-product-dummy and Geary-Khamis as well as elementary
indices Dutot, Carli, Harmonic mean, CSWD and Jevons. All of these
bilateral indices can be computed as period-on-period, fixed-base or
chained.

Multilateral indices can be computed in the time series context using
the GEKS, Weighted Time Product Dummy or Geary-Khamis indexes, and
updating is provided via the window, movement, half or mean splice
methods. The GEKS method is computed using either the Fisher or
Tornqvist superlative index number methods.

The package also provides functions to compute measures of dissimilarity
between time periods, which can be used to choose the linking period for
chained indices.

A sample dataset is included called CES\_sigma\_2, which is a dataset
containing prices and quantities on four products over twelve periods,
but additional datasets assuming different values of the elasticity of
substitution can be computed using the function `CESData`.

## Installation

You can install the released version of IndexNumR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("IndexNumR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("grahamjwhite/IndexNumR")
```

## Example

To estimate a simple chained Laspeyres price index using the
`CES_sigma_2` dataset,

``` r
priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",prodID = "prodID", indexMethod = "laspeyres", output = "chained")
```

A GEKS index with mean splicing and an 11 period window is estimated as
follows,

``` r
GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID", indexMethod = "tornqvist", window=11, splice = "mean")
```

More examples are contained in the package vignette.

``` r
vignette("indexnumr", package = "IndexNumR")
```
