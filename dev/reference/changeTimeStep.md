# Change the timestep of an output

This function changes the timestep of a table or an `antaresData` object
and performs the required aggregation or desaggregation. We can specify
(des)aggregate functions by columns, see the param `fun`.

## Usage

``` r
changeTimeStep(x, newTimeStep, oldTimeStep, fun = "sum", opts = simOptions())
```

## Arguments

- x:

  data.table with a column "timeId" or an object of class
  "antaresDataList"

- newTimeStep:

  Desired time step.The possible values are hourly, daily, weekly,
  monthly and annual.

- oldTimeStep:

  Current time step of the data. This argument is optional for an object
  of class `antaresData` because the time step of the data is stored
  inside the object

- fun:

  Character vector with one element per column to (des)aggregate
  indicating the function to use ("sum", "mean", "min" or "max") for
  this column. It can be a single element, in that case the same
  function is applied to every columns.

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

## Value

Either a data.table or an object of class "antaresDataList" depending on
the class of `x`

## Examples

``` r
if (FALSE) { # \dontrun{
setSimulationPath()

areasH <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1)
areasD <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1, timeStep ="daily")

areasDAgg <- changeTimeStep(areasH, "daily", "hourly")

all.equal(areasDAgg$LOAD, areasD$LOAD)

# Use different aggregation functions
mydata <- readAntares(select = c("LOAD", "MRG. PRICE"), timeStep = "monthly")
changeTimeStep(mydata, "annual", fun = c("sum", "mean"))
} # }
```
