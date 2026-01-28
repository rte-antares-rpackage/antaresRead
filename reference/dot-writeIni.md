# Write ini file from list obtain by antaresRead::readIniFile and modify by user

Write ini file from list obtain by antaresRead::readIniFile and modify
by user

## Usage

``` r
.writeIni(listData, pathIni, overwrite = FALSE)
```

## Arguments

- listData:

  `list`, modified list obtained by antaresRead::readIniFile.

- pathIni:

  `Character`, Path to ini file.

- overwrite:

  logical, should file be overwritten if already exist?

## Examples

``` r
if (FALSE) { # \dontrun{
pathIni <- "D:/exemple_test/settings/generaldata.ini"
generalSetting <- antaresRead::readIniFile(pathIni)
generalSetting$output$synthesis <- FALSE
writeIni(generalSetting, pathIni)
} # }
```
