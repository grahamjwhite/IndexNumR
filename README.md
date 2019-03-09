# IndexNumR

[![Build Status](https://travis-ci.org/grahamjwhite/IndexNumR.svg?branch=master)](https://travis-ci.org/grahamjwhite/IndexNumR)

[![Rdoc](http://www.rdocumentation.org/badges/version/IndexNumR)](http://www.rdocumentation.org/packages/IndexNumR)

An R package for computation of index numbers

IndexNumR provides a set of functions for computing various bilateral and multilateral indices. It is designed to compute price or quantity indices over time. Bilateral indices include Laspeyres, Paasche, Fisher, Tornqvist, Sato-Vartia, Walsh and CES, as well as elementary indices Dutot, Carli, Harmonic mean, CSWD and Jevons. All of these bilateral indices can be computed as period-on-period, fixed-base or chained. 

Multilateral indices can be computed in the time series context using the GEKS methodology, and updating is provided via the window, movement or mean splice methods. The GEKS method is computed using either the Fisher or Tornqvist superlative index number methods.

The package also provides functions to compute measures of dissimilarity between time periods, which can be used to choose the linking period for chained indices. 

To install the latest Github version of the package, you will first need devtools installed,
```R
install.packages("devtools")
```
then use the following R commands, 

```R
devtools::install_github("grahamjwhite/IndexNumR")  
library(IndexNumR)  
```
Or to include the vignette,
```R
devtools::install_github("grahamjwhite/IndexNumR", build_vignettes = TRUE)  
library(IndexNumR) 
```

## Examples

To estimate a simple chained Laspeyres price index using the `CES_sigma_2` dataset,

```R
priceIndex(CES_sigma_2,pvar = "prices",qvar = "quantities",pervar = "time",prodID = "prodID", indexMethod = "laspeyres", output = "chained")
```
A GEKS index with mean splicing and an 11 period window is estimated as follows,

```R
GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time", prodID = "prodID", indexMethod = "tornqvist", window=11, splice = "mean")
```
More examples are contained in the package vignette. 
