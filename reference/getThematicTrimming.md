# Get the Thematic trimming of an Antares study

![Antares API OK](figures/badge_api_ok.svg)

This function reads the "selection variables" section of the study's
"generaldata.ini" file.

Minimal version required is `v8.8`.

## Usage

``` r
getThematicTrimming(opts = simOptions())
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

## Value

`data.frame` with 2 columns :

- `variables` : names are displayed according to the study version

- `status_selection` : have 2 possible values ("active"; "skip")

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Thematic trimming of Antares study version >= v8.8
getThematicTrimming()
} # }
```
