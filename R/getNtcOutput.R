.getNtcOutputTs <- function(opts){
  ntc <- file.path(opts$simDataPath, "..", "ts-numbers", "ntc")
  area1 <- list.files(ntc)
  ll <- rbindlist(lapply(area1, function(XX){
    pathnct <- file.path(ntc, XX)
    areas2 <- list.files(pathnct)
    ll <- lapply(areas2, function(Y){
      Y2 <- gsub(".txt", "", Y)
      pathnctt <- file.path(pathnct, Y)
      tsId = as.numeric(readLines(pathnctt)[-1])
      data.table(tsId = tsId, areas = Y2, mcYear = 1:length(tsId))
    })  
    
    ll <- rbindlist(ll)
    
    
    ll$areas2 <- XX
    ll
  }))
  
  ll$link <-  paste0(ll$areas2, " - ", ll$areas)
  ll$areas <- ll$areas2 <- NULL
  ll
}  
