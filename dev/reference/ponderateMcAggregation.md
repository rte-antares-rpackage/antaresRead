# Mcyear aggregation weigthed by wd

Mcyear aggregation weigthed by wd

## Usage

``` r
ponderateMcAggregation(x, fun = weighted.mean, ...)
```

## Arguments

- x:

  `antaresData` data import with antaresRead

- fun:

  `function` function to use

- ...:

  `args` others args pass to fun

## Value

Object of class "antaresDataTable".

## Examples

``` r
if (FALSE) { # \dontrun{
  data <- readAntares(areas = 'all', mcYears = 'all')
  ponderateMcAggregation(data, fun = weighted.mean, w = c(.1, .9))

  
} # }
```
