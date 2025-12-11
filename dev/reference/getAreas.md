# Select and exclude areas

`getAreas` and `getDistricts` are utility functions that builds list of
areas or districts by using regular expressions to select and/or exclude
areas/districts

## Usage

``` r
getAreas(
  select = NULL,
  exclude = NULL,
  withClustersOnly = FALSE,
  regexpSelect = TRUE,
  regexpExclude = TRUE,
  opts = simOptions(),
  ignore.case = TRUE,
  districts = NULL
)

getDistricts(
  select = NULL,
  exclude = NULL,
  regexpSelect = TRUE,
  regexpExclude = TRUE,
  opts = simOptions(),
  ignore.case = TRUE
)
```

## Arguments

- select:

  Character vector. If `regexpSelect` is TRUE, this vector is
  interpreted as a list of regular expressions. Else it is interpreted
  as a list of area names. If `NULL`, all areas are selected

- exclude:

  Character vector. If `regexpExclude` is TRUE, this vector is
  interpreted as a list of regular expressions and each area validating
  one of them is excluded. Else it is interpreted as list of area names
  to exclude. If `NULL`, not any area is excluded.

- withClustersOnly:

  Should the function return only nodes containing clusters ?

- regexpSelect:

  Is `select` a list of regular expressions ?

- regexpExclude:

  Is `exclude` a list of regular expressions ?

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- ignore.case:

  Should the case be ignored when evaluating the regular expressions ?

- districts:

  Names of districts. If this argument is not null, only areas belonging
  to the specified districts are returned.

## Value

A character vector containing the name of the areas/districts satisfying
the rules defined by the parameters.

## See also

[`getLinks`](getLinks.md)
