#' @importFrom jsonlite read_json
.readjsonAntares <- function(path){
  X <- read_json(path)
  
  recustiveTF <- function(X){
    if(is.list(X)){
      lapply(X, recustiveTF)
    }else{
      if(!is.null(X)){
        if (X == "")return(NA)
        if (X == "true")return(TRUE)
        if (X == "false")return(FALSE)
        return(X)
      }
      return(X)
    }
  }
  
  recustiveTF(X)
}
