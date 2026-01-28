# Extract simulation options

The function [`readAntares`](readAntares.md) stores in its output the
options used to read some data (path of the study, area list, link list,
start date, etc.).

## Usage

``` r
simOptions(x = NULL)
```

## Arguments

- x:

  object of class `antaresTable` or `antaresData`

## Value

list of options used to read the data contained in an object or the last
simulation options read by [`setSimulationPath`](setSimulationPath.md)
if `x` is `NULL`

## Details

`simOptions` extracts these options from an object of class
`antaresTable` or `antaresOutput`. It can be useful when working on
multiple simulations, either to check how some object has been created
or to use it in some functions like [`getAreas`](getAreas.md) or
[`getLinks`](getLinks.md)

If the parameter of the function is `NULL`, it returns the default
simulation options, that is the options set by
[`setSimulationPath`](setSimulationPath.md) the last time it was run.

## Examples

``` r
if (FALSE) { # \dontrun{
  setSimulationPath(study1)
  
  simOptions() # returns the options for study 1
  
  data <- readAntares()
  
  # Choose a different study
  setSimulationPath(study2)
  
  simOptions() # returns the options for study 2
  
  getAreas() # returns the areas of the secund study
  getAreas(opts = simOptions(data)) # returns the areas of the first study
  
} # }
```
