## ---- echo=FALSE---------------------------------------------------------
  siZ <- readRDS(system.file("vignetteData/size.rds",package = "antaresRead"))
  TimeSel <- readRDS(system.file("vignetteData/Time.rds",package = "antaresRead"))

library(DT)
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 3, 'Advantage for storage')
    ),
    tr(lapply(c("", names(siZ)), th))
  )
  
))
sketch2 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 3, 'Speed of reading')
    ),
    tr(lapply(c("", names(TimeSel)), th))
  )
  
))


datatable(siZ,container = sketch, options = list(dom = 't',
                              columnDefs = list(list(className = 'dt-center', targets = 0:2))), 
          class = 'cell-border stripe')
datatable(TimeSel,container = sketch2,  options = list(dom = 't',
                              columnDefs = list(list(className = 'dt-center', targets = 0:2))), 
          class = 'cell-border stripe')



