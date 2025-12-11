# Read output for a list of short-term storage clusters

Read output for a list of short-term storage clusters

## Usage

``` r
readAntaresSTClusters(
  clustersST,
  selected = c("P.injection", "levels", "P.withdrawal"),
  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
  opts = simOptions(),
  parallel = FALSE,
  showProgress = TRUE
)
```

## Arguments

- clustersST:

  vector of short-term storage clusters to be imported

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

data.table of results for short-term storage clusters
