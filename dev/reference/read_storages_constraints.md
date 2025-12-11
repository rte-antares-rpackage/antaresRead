# Read Short-term storages / additional constraints

![Antares API OK](figures/badge_api_ok.svg)**\[experimental\]**

This function reads constraints of an Antares project (by area/cluster)
:

- Properties

- Time series

*Be aware that constraints are read in the input files of a study. So
they may have changed since a simulation has been run.*

## Usage

``` r
read_storages_constraints(opts = simOptions())
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

## Value

`list` with 2 sections per cluster/constraint (properties + values).

## Examples

``` r
if (FALSE) { # \dontrun{
# read/load an existing study (version >= 9.2)
setSimulationPath(path = "mypath/study")

read_storages_constraints()
} # }
```
