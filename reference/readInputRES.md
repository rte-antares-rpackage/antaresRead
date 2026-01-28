# Read Input RES time series

`readInputRes` is a function that reads renewable time series from an
antares project. But contrary to [`readAntares`](readAntares.md), it
only reads time series stored in the input folder, so it can work in
"input" mode.

## Usage

``` r
readInputRES(
  areas = "all",
  clusters,
  opts = simOptions(),
  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
  simplify = TRUE,
  parallel = FALSE,
  showProgress = TRUE
)
```

## Arguments

- areas:

  vector of RES areas names for which renewable time series must be
  read.

- clusters:

  vector of RES clusters names for which renewable time series must be
  read.

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- timeStep:

  Resolution of the data to import: hourly (default), daily, weekly,
  monthly or annual.

- simplify:

  If TRUE and only one type of output is imported then a data.table is
  returned. If FALSE, the result will always be a list of class
  "antaresData".

- parallel:

  Should the importation be parallelized ? (See details)

- showProgress:

  If TRUE the function displays information about the progress of the
  importation.

## Value

data.table with class "antaresDataTable".

## See also

[`setSimulationPath`](setSimulationPath.md),
[`readAntares`](readAntares.md), [`getAreas`](getAreas.md),
[`getLinks`](getLinks.md)
