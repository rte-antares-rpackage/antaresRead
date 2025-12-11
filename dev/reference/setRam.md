# Specify RAM limit

This function specify RAM limit (in Go) of the value returned by
[readAntares](readAntares.md).

## Usage

``` r
setRam(x)
```

## Arguments

- x:

  `numeric` RAM limit in Go

## Value

`list` (returned by [`options()`](https://rdrr.io/r/base/options.html))

## Examples

``` r
if (FALSE) { # \dontrun{
#Set maximum ram to used to 50 Go
setRam(50)
} # }
```
