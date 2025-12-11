# Copy data to the clipboard

`copyToClipboard` is a utility function that copies data to the
clipboard. The data can then be copied in another program like excel.

## Usage

``` r
copyToClipboard(x, ...)

# S3 method for class 'antaresDataList'
copyToClipboard(x, what, ...)
```

## Arguments

- x:

  an object used to select a method.

- ...:

  arguments passed to
  [`write.table`](https://rdrr.io/r/utils/write.table.html)

- what:

  character or numeric indicating which element to copy to clipboard
  (areas, links, clusters or districts)

## Value

The function does not return anything. It is only used to interact with
the clipboard

## Note

The function is useful only for small data objects: for a table, only
the 50000 rows are copied to clipboard. If the table to copy is longer,
either use filters to reduce the number of rows or write the table in
text file with [`write.table`](https://rdrr.io/r/utils/write.table.html)

## Examples

``` r
 # This only works on Windows systems
if (FALSE) { # \dontrun{
x <- data.frame(a = sample(10), b = sample(10))

copyToClipboard(x)

# Try to open excel and use CTRL + V to copy the data in a spreadsheet.
} # }
```
