# Creation of Mc_all new (only antares \> V6)

Creation of Mc_all new (only antares \> V6)

## Usage

``` r
parAggregateMCall(
  opts,
  nbcl = 8,
  verbose = 2,
  timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
  writeOutput = TRUE,
  mcWeights = NULL,
  mcYears = NULL,
  filtering = FALSE,
  selected = NULL,
  legacy = FALSE
)

aggregateResult(
  opts,
  verbose = 2,
  timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
  writeOutput = TRUE,
  mcWeights = NULL,
  mcYears = NULL,
  filtering = FALSE,
  selected = NULL,
  legacy = FALSE
)
```

## Arguments

- opts:

  `list` of simulation parameters returned by the function
  [setSimulationPath](setSimulationPath.md)

- nbcl:

  `numeric` Number of parralel process

- verbose:

  `numeric` show log in console. Defaut to 1

  - 0 : No log

  - 1 : Short log

  - 2 : Long log

- timestep:

  `character` antares timestep

- writeOutput:

  `boolean` write result or not.

- mcWeights:

  `numeric` vector of weigth for mcYears.

- mcYears:

  `numeric` mcYears to load.

- filtering:

  `boolean` filtering control

- selected:

  `list` named list (pass to antaresRead) : list(areas = 'a', links =
  'a - e')

- legacy:

  `boolean` run old version of the function

## Value

Object `list` of data.tables, each element representing one type of
element (areas, links, clusters)
