## Development version

### Bug fixes
- Input columns are now checked and converted in the same way by every function that takes a dataset (#84). Previously:
  - prices, quantities and time periods stored as factors were converted to their level codes rather than their values, which gave wrong results without an error. A factor time period also stopped the checks that the periods start at 1 and have no gaps from working;
  - `unitValues`, `imputeCarryPrices`, `imputeQuantities`, `shares`, `predictedShares`, `evaluateMatched`, `productChanges`, `valueDecomposition`, `groupIndexes` and `yearOverYearIndexes` did not check column types at all. For example, `unitValues` dropped periods 10 onwards when the time period was stored as character.
  
  Values that can't be converted to numbers are now an error that shows some of those values. Dates and fractional time periods are also an error. Unused levels of a factor product identifier are dropped, because they caused errors in `GKIndex` and `WTPDIndex` and created products that don't exist in `shares` and `imputeQuantities`.
- data.table and tibble inputs are now accepted. They are converted to a data frame internally; the input object is not modified (#84).
- Misspelled argument names passed to `priceIndex` or `quantityIndex` (e.g. `indexMmethod`) were silently absorbed by `...`, so the default Laspeyres index was returned. They are now an error that suggests the correct name. `groupIndexes` and `yearOverYearIndexes` also check the names in `indexArgs` (#83).
- `elasticity`, `relativeDissimilarity` and `mixScaleDissimilarity` gave wrong results when the rows of the data were not ordered consistently in each period. The data are now sorted before calculation.
- `priceIndex` and `GEKSIndex` with an unmatched sample could compare prices of different products when the products changed between periods. This is now an error for methods other than Dutot and TPD. Invalid `sample` values are also an error.
- `quantityIndex` ignored the `imputePrices` and `loweYoungBase` arguments.
- `priceIndex` with `basePeriod` other than 1 gave wrong results for period-on-period and chained output. `basePeriod` now only applies to fixed base output.
- The Sato-Vartia index and Montgomery indicator returned NaN when an expenditure share or value was unchanged between periods.
- `GEKSIndex` with `intGEKS = TRUE` failed when a bilateral comparison had no overlapping products.
- `GKIndex` with `solveMethod = "iterative"` failed with a matched sample when products were removed, and never warned when the iterations did not converge.
- Mixed-case argument values (e.g. `indexMethod = "Lowe"`, `splice = "Mean"`, `solveMethod = "Inverse"`, `chainMethod = "mixScale"`) passed validation but then failed.
- `imputeCarryPrices` corrupted factor product identifiers.
- `maximumSimilarityLinks` failed with two periods, and relied on the row order of its input.
- The bilateral TPD index failed when only one product was matched.
- `monthIndex` with `overlapWeeks = "majority"` could return an index starting at 0.
- `elasticity` now returns NA with a warning for periods where no solution is found, instead of stopping.
- The index functions now check that the time period variable starts at 1, and that a quantity variable is given for methods that need one.
- `GKIndex` now has a default window of 13, the same as `GEKSIndex` and `WTPDIndex`.
- Lowe and Young indexes with a base covering several periods (e.g. `loweYoungBase = 1:12`) gave wrong results if any product was missing from one of the base periods. The base quantities and expenditures are now summed for each product.
- `WTPDIndex` failed when matching left only one product in a window.

### Improvements
- `GEKSIndex` calculates each bilateral comparison once and reuses it across overlapping windows, which is several times faster (e.g. about 5 times faster for a 60 period, 200 product dataset with a window of 13).
- `WTPDIndex` and `GKIndex` use matrix calculations instead of loops over products and time periods. `WTPDIndex` in particular is much faster.
- The data for each time period is found without scanning the whole dataset, which speeds up `priceIndex` and the other functions that loop over time periods on large datasets.
- `imputeCarryPrices`, `unitValues` and `monthIndex` are vectorised and are much faster on large datasets. `imputeCarryPrices` keeps the column order and column types of the input data, and returns rows numbered from 1 in time period and product order. `unitValues` no longer converts numeric columns to character when the product identifier is character.
- Checking for duplicated time period and product combinations no longer builds a full time period by product table, which used a lot of memory for large scanner datasets.

## Package update 0.5.0 -> 0.6.0

### Improvements
- Package documentation updated to document the special sentinel "_PACKAGE"

### Bug fixes
- Fix minor issues with 'weekIndex' function around end of year (thanks to Alberto-Agudo, Issue #72) 

## Package update 0.4.0 -> 0.5.0

### Improvements
- `groupIndexes` to estimate group indexes using a group variable.
- `yearOverYearIndexes` to estimate year-over-year indexes.
- `predictedShareDissimilarity` to estimate the predicted share relative price dissimilarity measure. This has also been incorporated into `priceIndex` to allow estimation of indexes using similarity linking and the new measure. 
- `imputeCarryPrices` to fill in missing observations using carry forward and carry backward prices.
- parameter `solveMethod` to `GKIndex` now allows the GK equations to be solved by the iterative method. The matrix inverse method remains the default.

### Bug fixes
- `priceIndex` now allows estimation of elementary indexes on a dataset that does not contain quantities. Previously a quantity variable was always required, but elementary indexes do not need it (in fact it may not exist).
- fixed issue with `basePeriod` parameter of `priceIndex`.
- changed the way quantities and prices are calculated for the Dominicks dataset so that they represent unit prices and the associated quantity, instead of 'bundle' prices and quantities. 


## Package update 0.3.0 -> 0.4.0

### Improvements
- Added the Palgrave, Drobish, Stuvel, Marshall-Edgeworth, Lowe and Young bilateral indexes.
- Added the intersection GEKS (int-GEKS) method

### Bug fixes
- Fixed matching with bilateral fixed base indexes when products leave from, and later return to the sample. 


## Package update 0.2.0 -> 0.3.0

### Improvements

- Added the wisp, hasp and mean_pub splicing methods. These are the same methods as window, half and mean splice, but splice onto the published series instead of the previously calculated window. 
- Added the Walsh and Jevons indexes as bilateral indexes in the GEKSIndex function.
- Added a function for retrieving data from the Chicago Booth School of Business Dominicks Finer Foods dataset. 

### Bug fixes

- Fixed issue with the time-product-dummy regression when there are missing products
- Bug in the FBEW splice for the WTPD multilateral method
- Fixed an issue with the half-splice splicing method


## Package update 0.1.3 -> 0.2.0

### Improvements

- Added the bilateral time-product-dummy index number method. This optionally uses the Kennedy (1981) bias adjustment. 
- Added the Geary-Khamis bilateral index number method.
- The bilateral time-product-dummy method added as a valid bilateral index to use within the GEKS multilateral index number method.
- Added the Geary-Khamis and weighted time-product-dummy multilateral methods.
- Added the Fixed Base Expanding Window and Fixed Base Moving Window splicing methods.
- Added the function CESData() that calculates sample datasets using any value of sigma, assuming the data and methodology in Diewert and Fox (2017)
- Added basePeriod parameter to priceIndex() and quantityIndex() so it is now possible to choose the base period when using output = "fixedbase" for bilateral indices. 
- Added "new" and "leaving" columns to the count matrix as part of the evaluateMatched() function output. 

### Bug fixes

- Fixed an issue with evaluateMatched() incorrectly calculating matched expenditures/counts for the output = "fixedbase" option.

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

## Package update 0.1.1 -> 0.1.2

### Improvements
- Performance improvements to the monthIndex function for overlapWeeks = "majority" or "wholeOnly".  

### Bug fixes
- When matched sample is requested and no matches are found between the two periods, then for a bilateral index the corresponding element of the index is set to NA, and for a GEKS index the corresponding element of the bilateral comparison matrix is set to NA. A warning message tells the user which periods were set to NA. 

## Package update 0.1.0 -> 0.1.1

### Improvements
- Added additional argument "overlapWeeks" to monthIndex to allow flexibility in how the index is created. 
- Added more checks for correct parameter names in functions.

### Bug fixes
- Fixed bug in GEKSIndex function which affected indices computed with products that were initially in the sample, disappear for one or more periods, and then reappear later. 
