# Convert objects to antaresDataTable

This function converts a `data.frame` or a `data.table` into an
`antaresDataTable` object.

An `antaresDataTable` is simply a `data.table` with additional
attributes recording the time step, the type of data and the simulation
options.

## Usage

``` r
as.antaresDataTable(x, ...)

# S3 method for class 'data.frame'
as.antaresDataTable(x, synthesis, timeStep, type, opts = simOptions(), ...)
```

## Arguments

- x:

  object to convert to a an `antaresDataList`.

- ...:

  Arguments to be passed to methods.

- synthesis:

  Does the table contain synthetic results ?

- timeStep:

  Time step of the data. One of "hourly", "daily", "weekly", "monthly"
  or "annual".

- type:

  type of data: for instance "areas", "links", "clusters", etc.

- opts:

  Simulation options.

## Value

`antaresDataTable` object.
