# IndexNumR
An R package for computation of index numbers

IndexNumR provides a set of functions for computing various bilateral and multilateral indices. It is designed to compute price or quantity indices over time. Bilateral indices include Laspeyres, Paasche, Fisher, Tornqvist, Sato-Vartia and CES, as well as elementary indices Dutot, Carli, Harmonic mean, CSWD and Jevons. All of these bilateral indices can be computed as period-on-period, fixed-base or chained. 

Multilateral indices can be computed in the time series context using the GEKS methodology, and updating is provided via the window, movement or mean splice methods. The GEKS method is computed using either the Fisher or Tornqvist superlative index number methods.

The package also provides functions to compute measures of dissimilarity between time periods, which can be used to choose the linking period for chained indices. 

To install the package, you will first need devtools installed,
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
