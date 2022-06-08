
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IndexNumR

<!-- badges: start -->

[![Build
Status](https://github.com/grahamjwhite/IndexNumR/actions/workflows/check-standard.yaml/badge.svg?branch=master)](https://github.com/grahamjwhite/IndexNumR)

[![Codecov test
coverage](https://codecov.io/gh/grahamjwhite/IndexNumR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/grahamjwhite/IndexNumR?branch=master)

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
updating is provided via the window, movement, half, mean, fbew or fbmw
splicing methods. The GEKS method is computed using either the Fisher,
Tornqvist, Walsh, Jevons or TPD index number methods.

The package also provides functions to compute measures of dissimilarity
between time periods, which can be used to choose the linking period for
chained indices.

A sample dataset is included called CES\_sigma\_2, which is a dataset
containing prices and quantities on four products over twelve periods,
but additional datasets assuming different values of the elasticity of
substitution can be computed using the function `CESData`.

There is also a function `dominicksData` that can be used to download
the Dominicks Finer Foods scanner data from the [Chicago Booth School of
Business](https://www.chicagobooth.edu/research/kilts/datasets/dominicks).

## Installation

You can install the released version of IndexNumR from
[CRAN](https://cran.r-project.org) with:

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
library(IndexNumR)

priceIndex(CES_sigma_2, 
           pvar = "prices", 
           qvar = "quantities", 
           pervar = "time", 
           prodID = "prodID", 
           indexMethod = "laspeyres", 
           output = "chained")
#>            [,1]
#>  [1,] 1.0000000
#>  [2,] 0.9673077
#>  [3,] 1.2905504
#>  [4,] 1.3382002
#>  [5,] 1.2482444
#>  [6,] 1.7346552
#>  [7,] 1.6530619
#>  [8,] 1.4524186
#>  [9,] 1.8386215
#> [10,] 1.7126802
#> [11,] 2.1810170
#> [12,] 2.2000474
```

A GEKS index with mean splicing and an 11 period window is estimated as
follows,

``` r
GEKSIndex(CES_sigma_2, 
          pvar = "prices", 
          qvar = "quantities", 
          pervar = "time", 
          prodID = "prodID", 
          indexMethod = "tornqvist", 
          window = 11, 
          splice = "mean")
#>            [,1]
#>  [1,] 1.0000000
#>  [2,] 0.8927314
#>  [3,] 1.0776386
#>  [4,] 1.1127724
#>  [5,] 0.9310834
#>  [6,] 1.1785361
#>  [7,] 1.1219447
#>  [8,] 0.9380228
#>  [9,] 1.0951667
#> [10,] 0.9501914
#> [11,] 1.1277725
#> [12,] 1.1330748
```

More examples are contained in the package vignette.

``` r
vignette("indexnumr", package = "IndexNumR")
```
