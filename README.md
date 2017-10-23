# IndexNumR
An R package for computation of index numbers

IndexNumR provides a set of functions for computing various bilateral and multilateral indices. It is designed to compute price or quantity indices over time. Bilateral indices include Laspeyres, Paasche, Fisher, Tornqvist, as well as elementary indices Dutot, Carli and Jevons. All of these bilateral indices can be computed as fixed-base or chained. 

Multilateral indices can be computed in the time series context using the GEKS methodology, and updating is provided via the window, movement or mean splice methods. The GEKS method is computed using either the Fisher or Tornqvist superlative index number formulae.

The package also provides functions to compute measures of dissimilarity between time periods. The output can be used with the optrees package for R to compute minimum cost spanning trees, along the lines of Hill(2004). More generally, the dissimilarity measures can be used to choose the linking period for chained indices (with or without the use of a minimum cost spanning tree). 
