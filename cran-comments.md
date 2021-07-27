
## Package update 0.2.0 -> 0.3.0

### Improvements

- Added options for the weights when estimating the time-product-dummy index
- Added the wisp, hasp and mean_pub splicing methods. These are the same methods as window, half and mean splice, but splice onto the published series instead of the previously calculated window. 
- Added the Walsh and Jevons indexes as bilateral indexes in the GEKSIndex function.
- Added a function for retrieving data from the Chicago Booth School of Business Dominicks Finer Foods dataset. 

### Bug fixes

- Fixed issue with the time-product-dummy regression when there are missing products
- Bug in the FBEW splice for the WTPD multilateral method
- Fixed an issue with the half-splice splicing method


## Test environments

* macOS, ubuntu and Windows (via Github Actions)
* R 4.1.0 (all OS) and r-devel (on Ubuntu) 

## R CMD check results
0 errors v | 0 warnings v | 0 notes v 

## Downstream dependencies

I have also checked 1 reverse dependency of IndexNumR with no issues
