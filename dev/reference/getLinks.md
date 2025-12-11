# Retrieve links connected to a set of areas

This function finds the names of the links connected to a set of areas.

## Usage

``` r
getLinks(
  areas = NULL,
  exclude = NULL,
  opts = simOptions(),
  internalOnly = FALSE,
  namesOnly = TRUE,
  withDirection = FALSE,
  withTransmission = FALSE
)
```

## Arguments

- areas:

  Vector containing area names. It represents the set of areas we are
  interested in. If `NULL`, all areas of the study are used.

- exclude:

  Vector containing area names. If not `NULL`, all links connected to
  one of these areas are omitted.

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- internalOnly:

  If `TRUE`, only links that connect two areas from parameter `areas`
  are returned. If not, the function also returns all the links that
  connect an area from the list with an area outside the list.

- namesOnly:

  If `TRUE`, the function returns a vector with link names, else it
  returns a table containing the name, the origin and the destination of
  each selected link.

- withDirection:

  Used only if `namesOnly = FALSE`. If `FALSE`, then the function
  returns a table with one line per link, containing the link name, the
  origin and the destination of the link. If `TRUE`, then it returns a
  table with columns `area`, `link`, `to` and `direction` which is equal
  is equal to 1 if the link connects `area` to `to` and -1 if it
  connects `to` to `area`. The column `area` contains only areas that
  are compatible with parameters `areas` and `exclude`. Note that the
  same link can appear twice in the table with different directions.

- withTransmission:

  Used only if `namesOnly = FALSE`. If `TRUE`, a column is added to
  indicate type of transmission capacities for links.

## Value

If `namesOnly = TRUE` the function returns a vector containing link
names

If `namesOnly = FALSE` and `withDirection = FALSE`, it returns a
`data.table` with **exactly one line per link** and with three columns:

- link:

  Link name

- from:

  First area connected to the link

- to:

  Second area connected to the link

If `namesOnly = FALSE` and `withDirection = TRUE`, it returns a
`data.table` with **one or two lines per link** and with four columns:

- area:

  Area name

- link:

  Link name

- to:

  Area connected to `area` by `link`

- direction:

  1 if the link connects `area` to `to` else -1

## Examples

``` r
if (FALSE) { # \dontrun{

# Get all links of a study
getLinks()

# Get all links with their origins and destinations
getLinks(namesOnly = FALSE)

# Get all links connected to French areas (assuming their names contain "fr")
getLinks(getAreas("fr"))

# Same but with only links connecting two French areas
getLinks(getAreas("fr"), internalOnly = TRUE)

# Exclude links connecting real areas with pumped storage virtual areas
# (assuming their names contain "psp")
getLinks(getAreas("fr"), exclude = getAreas("psp"))

} # }
```
