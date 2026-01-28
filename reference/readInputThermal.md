# Read Input thermal time series

`readInputThermal` is a function that reads thermal time series from an
antares project. But contrary to [`readAntares`](readAntares.md), it
only reads time series stored in the input folder, so it can work in
"input" mode.

## Usage

``` r
readInputThermal(
  areas = "all",
  clusters,
  thermalAvailabilities = TRUE,
  thermalModulation = FALSE,
  thermalData = FALSE,
  opts = simOptions(),
  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
  simplify = TRUE,
  parallel = FALSE,
  showProgress = TRUE
)
```

## Arguments

- areas:

  vector of areas names for which thermal time series must be read.

- clusters:

  vector of clusters names for which thermal time series must be read.

- thermalAvailabilities:

  if TRUE, return thermalAvailabilities data

- thermalModulation:

  if TRUE, return thermalModulation data

- thermalData:

  if TRUE, return thermalData from prepro

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

If thermalModulation or thermalData is TRUE, an object of class
"antaresDataList" is returned. It is a list of data.tables for selected
input

Else the result is a data.table with class "antaresDataTable".

## Note

the clusters parameter can also accept the special value "all". It
indicates the function to read the desired time series for all clusters.

## See also

[`setSimulationPath`](setSimulationPath.md),
[`readAntares`](readAntares.md), [`getAreas`](getAreas.md),
[`getLinks`](getLinks.md)
