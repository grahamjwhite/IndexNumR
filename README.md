# IndexNumR
An R package for computation of index numbers

IndexNumR provides a set of functions for computing various bilateral and multilateral indices. It is designed to compute price or quantity indices over time. Bilateral indices include Laspeyres, Paasche, Fisher, Tornqvist, Sato-Vartia, as well as elementary indices Dutot, Carli, Harmonic mean, CSWD and Jevons. All of these bilateral indices can be computed as fixed-base or chained. 

Multilateral indices can be computed in the time series context using the GEKS methodology, and updating is provided via the window, movement or mean splice methods. The GEKS method is computed using either the Fisher or Tornqvist superlative index number formulae.

The package also provides functions to compute measures of dissimilarity between time periods, which can be used to choose the linking period for chained indices. 

To install the package, use the following R commands, 

```R
install.packages("devtools")  
devtools::install_github("grahamjwhite/IndexNumR")  
library(IndexNumR)  
```
