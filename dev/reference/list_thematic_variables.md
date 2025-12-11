# List of thematic trimming variables available according to study version

Minimal version required is v8.8

## Usage

``` r
list_thematic_variables(opts = simOptions())
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

## Value

`data.frame` of available columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Display list (use first `setSimulationPath()` to have an active study loaded)
list_thematic_variables()
} # }
```
