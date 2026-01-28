# Read output for a list of areas

This a function is a wrapper for "antaresData" that reads all data for a
list of areas.

## Usage

``` r
readAntaresAreas(
  areas,
  links = TRUE,
  clusters = TRUE,
  clustersRes = TRUE,
  internalOnly = FALSE,
  opts = simOptions(),
  ...
)
```

## Arguments

- areas:

  Vector containing area names. It represents the set of areas we are
  interested in. If `NULL`, all areas of the study are used.

- links:

  should links connected to the areas be imported ?

- clusters:

  should the thermal clusters of the areas be imported ?

- clustersRes:

  should the renewable clusters of the areas be imported ?

- internalOnly:

  If `TRUE`, only links that connect two areas from parameter `areas`
  are returned. If not, the function also returns all the links that
  connect an area from the list with an area outside the list.

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- ...:

  Other arguments passed to the function [`readAntares`](readAntares.md)

## Value

If `simplify = TRUE` and only one type of output is imported then the
result is a data.table.

Else an object of class "antaresData" is returned. It is a list of
data.tables, each element representing one type of element (areas,
links, clusters)

## Examples

``` r
if (FALSE) { # \dontrun{
myarea <- simOptions()$areaList[1]
data <- readAntaresAreas(myarea)

# Equivalent but more concise than:
data2 <- readAntares(myarea, links = getLinks(myarea), clusters = myarea)

all.equal(data, data2)
} # }
```
