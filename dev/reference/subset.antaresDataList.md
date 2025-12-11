# Subset an antaresDataList

Subset method for `antaresDataList`.

## Usage

``` r
# S3 method for class 'antaresDataList'
subset(x, y = NULL, areas = NULL, timeIds = NULL, mcYears = NULL, ...)
```

## Arguments

- x:

  Object of class `antaresDataList` created with
  [`readAntares`](readAntares.md).

- y:

  A table containing at least one of the columns "area", "timeId" or
  "mcYear". If it is not `NULL`, then only tuples
  `(area, timeId, mcYear)` present in this table are kept.

- areas:

  Vector of area names to keep in the result. If `NULL`, all areas are
  kept.

- timeIds:

  Vector of time ids to keep. If `NULL`, all time ids are kept.

- mcYears:

  Vector of monte-carlo years to keep. If `NULL`, all time ids are kept.

- ...:

  Currently unused.

## Value

A filtered `antaresDataList`.

## Examples

``` r
if (FALSE) { # \dontrun{
#keep only the first year
mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
mySubset<-subset(mydata, mcYears = 1)
  
#keep only the first year for areas a and b 
mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
mySubset<-subset(mydata, mcYears = 1, areas=c("a", "b")) 

#' #keep only the first year for areas a and b and timeIds include in 5:16 
mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
mySubset<-subset(mydata, mcYears = 1, areas=c("a", "b"), timeIds=5:16) 
  
} # }
```
