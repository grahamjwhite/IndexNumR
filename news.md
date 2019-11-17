## Package update 0.1.2 -> 0.1.3

### Improvements
- The column types of the input dataframe (for the time period, prices and quantities) are checked and converted to numeric if they aren't already.  

### Bug fixes
- The input data frame is now sorted prior to calculation. This solves an error where the products are passed to the priceIndex function in a different order in each time period, resulting in incorrectly matched product data when computing the index. 

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
