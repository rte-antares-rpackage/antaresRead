#' Subset an antaresDataList
#' 
#' Subset method for \code{antaresDataList}.
#' 
#' @param x Object of class \code{antaresDataList} created with 
#'   \code{\link{readAntares}}.
#' @param y A table containing at least one of the columns "area", "timeId" or 
#'   "mcYear". If it is not \code{NULL}, then only tuples \code{(area, timeId,
#'   mcYear)} present in this table are kept.
#' @param areas Vector of area names to keep in the result. If \code{NULL}, all 
#'   areas are kept.
#' @param timeIds Vector of time ids to keep. If \code{NULL}, all time ids are 
#'   kept.
#' @param mcYears Vector of monte-carlo years to keep. If \code{NULL}, all time 
#'   ids are kept.
#' @param ... Currently unused.
#' 
#' @return 
#' A filtered \code{antaresDataList}.
#' 
#' @examples
#' \dontrun{
#' #keep only the first year
#' mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
#' mySubset<-subset(mydata, mcYears = 1)
#'   
#' #keep only the first year for areas a and b 
#' mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
#' mySubset<-subset(mydata, mcYears = 1, areas=c("a", "b")) 
#' 
#' #' #keep only the first year for areas a and b and timeIds include in 5:16 
#' mydata <- readAntares(areas = "all", links = "all", mcYears = "all")
#' mySubset<-subset(mydata, mcYears = 1, areas=c("a", "b"), timeIds=5:16) 
#'   
#' }
#' 
#' @export
#' 
subset.antaresDataList <- function(x, y = NULL, areas = NULL, timeIds = NULL, mcYears = NULL, ...) {
 
  
  #check parameter areas
  if (!is.null(areas)){
      for(i in areas){
        if(!(i %in% getAreas())){
          stop(sprintf('area "%s" is not an area of this study', i))
        }
      }
  }
  
  #check parameter mcYears
  if (!is.null(timeIds)){
    tIds<-c()
    for(j in names(x)){
      tIds<-unique(c(tIds,unique(x[[j]]$timeId)))
    }
    
    for(i in timeIds){
      if(!(i %in% tIds)){
        stop(sprintf('timeId %i is not an timeId of this study', i))
      }
    }  
  }
  
  #check parameter mcYears
  if (!is.null(mcYears)){
    Mc<-c()
    for(j in names(x)){
      Mc<-unique(c(Mc,unique(x[[j]]$mcYear)))
    }
    
    for(i in mcYears){
      if(!(i %in% Mc)){
        stop(sprintf('McYear %i is not an McYear of this study', i))
      }
    }  
  }

  if (!is.null(y)) {
    y <- as.data.table(y)
    
    filterCols <- intersect(names(y), c("area", "timeId", "mcYear"))
    if (length(filterCols) == 0) stop("y should be null or contain at least one of the columns 'area', 'timeId' or 'mcYear'")
    yarea <- unique(y[, filterCols, with = FALSE], by = NULL)
    
    if (is.null(yarea$area)) ylink <- yarea
    else {
      bycols <- setdiff(filterCols, "area")
      if (length(bycols) == 0) {
        ylink <- data.table(link = getLinks(yarea$area))
      } else {
        ylink <- yarea[, .(link = getLinks(area)), by = bycols]
      }
      
    }
  }
  
  for (n in names(x)) {
    
    if (!is.null(x[[n]]$area) & !is.null(y)) {
      bycols <- intersect(names(x[[n]]), names(yarea))
      if (length(bycols) > 0) {
        x[[n]] <- merge(x[[n]], unique(yarea, by = NULL), by = bycols)
      }
    }
    
    if (!is.null(x[[n]]$link) && !is.null(y)) {
      bycols <- intersect(names(x[[n]]), names(ylink))
      if (length(bycols) > 0) {
        x[[n]] <- merge(x[[n]], unique(ylink, by = NULL), by = bycols)
      }
    }
    
    filter <- TRUE
    if (!is.null(areas)) {
      if (!is.null(x[[n]]$area)) {
        filter <- filter & x[[n]]$area %in% areas
      } else if (!is.null(x[[n]]$link)) {
        filter <- filter & x[[n]]$link %in% getLinks(areas, opts = simOptions(x))
      }
    }
    if (!is.null(timeIds)) {
      filter <- filter & x[[n]]$timeId %in% timeIds
    }
    if (!is.null(mcYears)) {
      filter <- filter & x[[n]]$mcYear %in% mcYears
    }
    x[[n]] <- x[[n]][filter, ]
  }
  
  x
}
