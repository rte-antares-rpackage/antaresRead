# Release 2.9.3
R version 4.1.2 (2021-11-01)  
Platform: x86_64-pc-linux-gnu (64-bit)  
Running under: Ubuntu 22.04.4 LTS

❯ checking for unstated dependencies in examples ... OK
   WARNING
  ‘qpdf’ is needed for checks on size reduction of PDFs

0 errors ✔ | 1 warning ✖ | 0 notes ✔

local warning only, not printed on Github CI 

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

Issue has been opened (`#389`) 


# Relase 2.9.0  
  -  Many fixes 
## R CMD check results OK

0 errors  | 0 warnings | 0 notes 

## rev dep check OK

── CHECK 3 packages ──
✔ antaresEditObject 0.7.1                ── E: 0     | W: 0     | N: 0                                                            
✔ antaresProcessing 0.18.3               ── E: 0     | W: 0     | N: 0                                                            
✔ antaresViz 0.18.3                      ── E: 0     | W: 0     | N: 0          


# Release 2.7.1 

 - To fix problem with cran check (see log "M1mac").  
 - Add some bug fix (see news.md)  
 - Fix multiple "path" bug in `setSimulationPath()`  
 - Manage package :  
    - Add `\value` section in documentation  
    - Using foo::f instead of foo:::f to access exported objects  
    - Replace `options(warn=-1)` by `suppressWarnings()`  
    - Delete references to .GlobalEnv (`assign()`)
 


## R CMD check results OK

0 errors  | 0 warnings | 0 notes 

## rev dep check OK

── CHECK ─────── 3 packages ──
✔ antaresEditObject 0.7.0                ── E: 0     | W: 0     | N: 0                                                                   
✔ antaresProcessing 0.18.2               ── E: 0     | W: 0     | N: 0                                                                   
✔ antaresViz 0.18.1   


