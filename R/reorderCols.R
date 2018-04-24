#Copyright © 2016 RTE Réseau de transport d’électricité

# Private function that returns the name of de id columns of a table
.idCols <- function(x, removeTimeId = FALSE) {
  idCol <- intersect(pkgEnv$idVars, names(x))
  if(removeTimeId){
    idCol <- idCol[which(!idCol%in%pkgEnv$idTimeVars)]
  }
  idCol
}

#' reorder the columns of a data table
#' 
#' This function puts the id columns in the order defined by variable pkgEnv$idVars,
#' and then the other columns in the same order as in 'x'
#' 
#' @noRd
#' 
.reorderCols <- function(x) {
  idCols <- .idCols(x)
  neworder <- c(idCols, setdiff(names(x), idCols))
  setcolorder(x, neworder)
  invisible(x)
}

#' reorder the row of inputTSHydro
#' 
#' This function reorder the data of hydroStorage according to parameters "first day" and "last day"
#' 
#' @noRd
#' 
#' 
.reorderInputTSHydroStorage<-function(inputTSHydro=NULL, path=NULL, opts=NULL){
  if(is.null(inputTSHydro) | is.null(path) | is.null(opts) ){
    stop("One parameter is missing")
  }else if(grepl(pattern = "hydro", x=path) & grepl(pattern = "mod", x=path)){
    firstMonth<-lubridate::month(opts$start)
    inputTSTemp<-copy(inputTSHydro)
    rowNum<-NULL
    inputTSTemp[, c("rowNum"):=1:12]
    inputTSFirstPart<-inputTSTemp[rowNum >= firstMonth,]
    inputTSFirstPart<-inputTSFirstPart[rowNum >= firstMonth, rowNum:=as.integer(rowNum-(firstMonth-1))]
    inputTSSecondPart<-inputTSTemp[rowNum < firstMonth,]
    inputTSSecondPart<-inputTSSecondPart[rowNum < firstMonth, rowNum:=as.integer(rowNum+(firstMonth-1))]
    inputTSHydro<-rbind(inputTSFirstPart, inputTSSecondPart)   
    inputTSHydro<-inputTSHydro[, rowNum:=NULL]
  }
  
  invisible(inputTSHydro)
}
