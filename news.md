## Package update 0.1.0 -> 0.1.1

### Improvements
- Added additional argument "overlapWeeks" to monthIndex to allow flexibility in how the index is created. 
- Added more checks for correct parameter names in functions.

### Bug fixes
- Fixed bug in GEKSIndex function which affected indices computed with products that were initially in the sample, disappear for one or more periods, and then reappear later. 
