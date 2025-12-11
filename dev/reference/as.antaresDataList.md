# Convert objects to antaresDataTable

This function converts a list of tables or table into an
`antaresDataList` object.

An `antaresDataList` is a list of tables of class`antaresDataTable`. It
also has attributes that store the time step, the type of data and the
simulation options.

## Usage

``` r
as.antaresDataList(x, ...)

# S3 method for class 'antaresDataTable'
as.antaresDataList(x, name = NULL, ...)

# S3 method for class 'data.frame'
as.antaresDataList(
  x,
  synthesis,
  timeStep,
  type,
  opts = simOptions(),
  name = type,
  ...
)
```

## Arguments

- x:

  Data.frame or data.table to convert to a an antaresDataTable.

- ...:

  Arguments to be passed to methods.

- name:

  name of the table in the final object. If `NULL`, the type of the data
  is used.

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

`antaresDataList` object.
