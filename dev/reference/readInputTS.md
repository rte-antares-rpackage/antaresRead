# Read Input time series

![Antares API OK](figures/badge_api_ok.svg)

`readInputTS` is a function that reads time series from an antares
project. But contrary to [`readAntares`](readAntares.md), it only reads
time series stored in the input folder, so it can work in "input" mode.

## Usage

``` r
readInputTS(
  load = NULL,
  thermalAvailabilities = NULL,
  ror = NULL,
  mingen = NULL,
  hydroStorage = NULL,
  hydroStorageMaxPower = NULL,
  wind = NULL,
  solar = NULL,
  misc = NULL,
  reserve = NULL,
  linkCapacity = NULL,
  resProduction = NULL,
  st_storage = NULL,
  opts = simOptions(),
  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
  simplify = TRUE,
  parallel = FALSE,
  showProgress = TRUE
)
```

## Arguments

- load:

  vector of areas names for which load time series must be read.

- thermalAvailabilities:

  vector of areas names for which thermal availabilities of clusters
  must be read.

- ror:

  vector of areas names for which run of river time series must be read.

- mingen:

  vector of areas names for which Hydro Pmin time series must be read.
  (only for Antares version \>= 860)

- hydroStorage:

  vector of areas names for which hydrolic storage time series must be
  read.

- hydroStorageMaxPower:

  vector of areas names for which hydrolic storage maximum power time
  series must be read.

- wind:

  vector of areas names for which wind time series must be read

- solar:

  vector of areas names for which solar time series must be read

- misc:

  vector of areas names for which misc time series must be read

- reserve:

  vector of areas names for which reserve time series must be read

- linkCapacity:

  vector of links names for which links characteristics time series must
  be read

- resProduction:

  vector of areas names for which renewables clusters production time
  series must be read.

- st_storage:

  vector of areas names for which st-storage clusters production time
  series must be read.

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

If `simplify = TRUE` and only one type of input is imported then the
result is a data.table with class "antaresDataTable".

Else an object of class "antaresDataList" is returned. It is a list of
data.tables, each element representing one type of element (load, wind,
solar, etc.).

## Note

All parameters expecting a vector of areas or links names also accept
the special value "all". It indicates the function to read the desired
time series for all areas or links.

## See also

[`setSimulationPath`](setSimulationPath.md),
[`readAntares`](readAntares.md), [`getAreas`](getAreas.md),
[`getLinks`](getLinks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set an antares study in "input" mode. This is useful when one want to
# inspect input time series before running a simulation.
# Note that readAntares do not function in input mode, but readInputTS
# works with any mode.

setSimulationPath("path_to_the_study", "input")

# Read load time series
readInputTS(load = "all")

# Read hydrolic storage and maximum power in the same call:
readInputTS(hydroStorage = "all", hydroStorageMaxPower = "all")

# Use a different time step
myArea <- readInputTS(load= "myArea", timeStep = "monthly")

# Quick plot to visualize the variability of the series
matplot(myArea[, - (1:2), with = FALSE], type = "l")
} # }
```
