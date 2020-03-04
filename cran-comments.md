## Package update 0.1.2 -> 0.1.3

### Improvements
- The column types of the input dataframe (for the time period, prices and quantities) are checked and converted to numeric if they aren't already.
- Price and volume indicators can now be computed using Laspeyres, Paasche, Bennet and Montgomery methods, via the functions priceIndicator() and volumeIndicator(). 
- Price and volume indicators can be used to decompose value change via the function valueDecomposition(). 
- A new function, values(), can now be used to compute the values (prices*quantities) in each period, with optional matched sample. Options are available for how the matching is conducted.  
- Two new bilateral indexes are now available, the Geometric Laspeyres and the Geometric Paasche. 

### Bug fixes
- The input data frame is now sorted prior to calculation. This solves an error where the products are passed to the priceIndex function in a different order in each time period, resulting in incorrectly matched product data when computing the index. 
- The output data frame of the unitValues function no longer returns factors (i.e., stringsAsFactors = FALSE), which could break subsequent calculations.

## Test environments

* local Windows 10 Pro install, R version 3.6.2 and R-devel
* Ubuntu 16.04.6 (on travis-ci), R version 3.6.2

## R CMD check results
0 errors v | 0 warnings v | 0 notes v 

## Downstream dependencies

* No downstream dependencies.
