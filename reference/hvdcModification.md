# hvdc straitement

usage for hvdc

## Usage

``` r
hvdcModification(data, removeHvdcAreas = TRUE, reafectLinks = FALSE)
```

## Arguments

- data:

  `antaresDataList` data to apply straitement

- removeHvdcAreas:

  `boolean` remove HVDC areas.

- reafectLinks:

  `boolean` .

## Value

Object of class "antaresDataList" is returned. It is a list of
data.tables, each element representing one type of element (areas,
links, clusters)

## Examples

``` r
if (FALSE) { # \dontrun{

data <- readAntares(areas = 'all', links = 'all')
data <- setHvdcAreas(data, "psp in")
data <- hvdcModification(data)

} # }
```
