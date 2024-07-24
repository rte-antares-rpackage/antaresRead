
# Release 2.7.1 

 - To fix problem with cran check (see log "M1mac").  
 - Add some bug fix (see news.md)  
 - Fix multiple "path" bug in `setSimulationPath()`  
 - Manage package :  
    - Add `\value` section in documentation  
    - Using foo::f instead of foo:::f to access exported objects  
    - Replace `options(warn=-1)` by `suppressWarnings()`
 


## R CMD check results OK

0 errors  | 0 warnings | 0 notes 

## rev dep check OK

── CHECK ─────── 3 packages ──
✔ antaresEditObject 0.7.0                ── E: 0     | W: 0     | N: 0                                                                   
✔ antaresProcessing 0.18.2               ── E: 0     | W: 0     | N: 0                                                                   
✔ antaresViz 0.18.1   


