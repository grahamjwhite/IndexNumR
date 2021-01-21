## Resubmission 2

- Fixed hyperlink
- Added a reference to a paper in the description that covers a number of the methods in the package. 
- win-builder is showing 1 NOTE for possibly misspelled words in description ("CCDI", "Geary", "Khamis", "Tornqvist"). These are NOT misspelled.  

## Resubmission

- Fixed NOTE ("Change to non-FOSS package license"): an incorrect change was made to the license, and this has been corrected. License continues to be GPL-2.
- Fixed hyperlink (http -> https) in README.md. 


## Package update 0.1.3 -> 0.2

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


## Test environments

* local Windows 10 Pro install, R version 4.0.3 and R-devel
* Ubuntu 16.04.6 (on travis-ci), R version 4.0.2

## R CMD check results
0 errors v | 0 warnings v | 0 notes v 

## Downstream dependencies

I have also run R CMD Check on reverse dependencies of IndexNumR
