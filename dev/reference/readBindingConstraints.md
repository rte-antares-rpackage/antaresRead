# Read binding constraints

![Antares API OK](figures/badge_api_ok.svg)**\[experimental\]**

This function reads the binding constraints of an Antares project.

Be aware that binding constraints are read in the input files of a
study. So they may have changed since a simulation has been run.

## Usage

``` r
readBindingConstraints(
  opts = simOptions(),
  with_time_series = TRUE,
  constraint_names = NULL
)
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- with_time_series:

  `boolean` if TRUE, the second member time series are read

- constraint_names:

  `str` constraint names to filter on

## Value

An object of class `bindingConstraints`. This object is also a named
list with 3 sections per read constraint.

## Note

For an study Antares **version \>=8.7.0**. Now contains `data.frame`
with one line per time step and \\p\\ colums according to "scenarized
RHS".

For "both" case, you will find in section `values` two `data.frame` :

- One `data.frame` for `less`

- One `data.frame` for `greater`

For an study Antares **version \<8.7.0**.

Section `values` contains one line per time step and three columns
"less", "greater" and "equal"

## Warning

Since `release 2.7.0` the structure of the returned object has evolved
for all versions of study Antares:

- .ini parameters are in section `properties`

- Coefficients links or thermal are in section `coefs`

- Values are already in section `values`

## Examples

``` r
if (FALSE) { # \dontrun{
setSimulationPath()

constraints <- readBindingConstraints()

# read properties
constraints$properties

# read coefs
constraints$coefs

# read values
constraints$values
  # both case ( study Antares >=8.7.0)
constraints$values$less
constraints$values$greater

# display equation (only for study Antares <8.7.0)
summary(constraints)

# read binding constraints without the time series
readBindingConstraints(opts = simOptions(), with_time_series = FALSE)
} # }
```
