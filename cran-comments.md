
## Resubmission

* Fixed relocated URL. 

## Package update 0.4.0 -> 0.5.0

### Improvements
- `groupIndexes` to estimate group indexes using a group variable.
- `yearOverYearIndexes` to estimate year-over-year indexes.
- the predicted share relative price dissimilarity measure has been added to the `relativeDissimilarity` function. This dissimilarity measure has also been incorporated into `priceIndex` to allow estimation of indexes using similarity linking and the new measure. 
- `imputeCarryPrices` to fill in missing observations using carry forward and carry backward prices.
- parameter `solveMethod` to `GKIndex` now allows the GK equations to be solved by the iterative method. The matrix inverse method remains the default.

### Bug fixes
- `priceIndex` now allows estimation of elementary indexes on a dataset that does not contain quantities. Previously a quantity variable was always required, but elementary indexes do not need it (in fact it may not exist).
- fixed issue with `basePeriod` parameter of `priceIndex`.
- changed the way quantities and prices are calculated for the Dominicks dataset so that they represent unit prices and the associated quantity, instead of 'bundle' prices and quantities. 

## Test environments

* macOS, ubuntu and Windows (via Github Actions)
* R 4.1.2 (all OS) and r-devel (on Ubuntu) 

## R CMD check results
0 errors v | 0 warnings v | 0 notes v 

## Downstream dependencies

I have also checked 2 reverse dependencies of IndexNumR with no issues
