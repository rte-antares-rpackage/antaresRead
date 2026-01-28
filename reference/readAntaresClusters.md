# Read output for a list of clusters

Read output for a list of clusters

## Usage

``` r
readAntaresClusters(
  clusters,
  selected = c("production", "NP Cost", "NODU", "profit"),
  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
  opts = simOptions(),
  parallel = FALSE,
  showProgress = TRUE
)
```

## Arguments

- clusters:

  vector of thermal clusters to be imported

- selected:

  vector of thematic trimming

- timeStep:

  Resolution of the data to import: hourly (default), daily, weekly,
  monthly or annual.

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- parallel:

  Should the importation be parallelized ? (See details)

- showProgress:

  If TRUE the function displays information about the progress of the
  importation.

## Value

data.table of results for thermal clusters
