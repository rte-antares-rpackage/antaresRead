# Read Optimization Criteria

This function can be used to read the value of the criteria optimized by
ANTARES. Notice that these values are only available in "Xpansion" mode
or when option "Export mps" is turned on.

## Usage

``` r
readOptimCriteria(opts = simOptions())
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

## Value

A table of class `antaresDataTable`. It contains the usual columns
`timeID`, `mcYear`, `time` and two columns "criterion1" and "criterion2"
containing the values of the criteria. Time step can be daily or weekly
depending on the optimization options.

## Examples

``` r
if (FALSE) { # \dontrun{
setSimulationPath()

optimCriteria <- readOptimCriteria()
} # } 
```
