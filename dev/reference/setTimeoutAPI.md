# Change API Timeout

Change API Timeout

## Usage

``` r
setTimeoutAPI(opts, timeout)
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPathAPI`](setSimulationPath.md)

- timeout:

  `numeric` API timeout (seconds). Default to 600.

## Value

Object of class `simOptions`, list of options used to read the data
contained in the last simulation read by
[`setSimulationPathAPI`](setSimulationPath.md).

## Examples

``` r
if (FALSE) { # \dontrun{
opts <- setTimeoutAPI(opts, timeout = 45)
} # }
```
