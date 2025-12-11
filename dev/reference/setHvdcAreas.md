# Set hvdc areas

This function add hvdc attribute

## Usage

``` r
setHvdcAreas(data, areas)
```

## Arguments

- data:

  `antaresData` or `antaresDatalist` data.

- areas:

  `character` hvdc areas list.

## Value

Object of class "antaresDataList" is returned. It is a list of
data.tables, each element representing one type of element (areas,
links, clusters)

## Examples

``` r
if (FALSE) { # \dontrun{

library(antaresRead)
opts <- setSimulationPath('mypath', 1)
myAreaOutput <- readAntares(areas = "all", links = "all")
myAreaOutput <- setHvdcAreas(myAreaOutput, "y_dsr")


} # }
```
