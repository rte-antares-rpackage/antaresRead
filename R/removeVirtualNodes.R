#' Remove virtual nodes
#' 
#' This function removes virtual nodes from an \code{antaresOutput} object and
#' corrects the data for the real nodes. The \code{antaresOutput} object should
#' contain node and link data to function correctly. 
#' 
#' @param x
#'   An object of class \code{antaresOutput} with at least components 
#'   \code{nodes} and \code{links}
#' @param spec
#'   A list containing the specification of the corrections to perform 
#'   (see details)
#'   
#' @return 
#' an \code{antaresOutput object} that has been corrected according to the
#' specifications of spec. 
#' 
#' @examples 
#' \dontrun{
#' data <- readAntares(nodes="all", links="all")
#' 
#' # Remove pumped storage virtual nodes
#' 
#' spec <- list(
#'   list(
#'     type = "pumped storage",
#'     trueNodes = "B",
#'     virtualNodes = c("psp in", "psp out")
#'   )
#' )
#' 
#' correctedData <- removeVirtualNodes(data, spec)
#' }
#' 
#' @export
#' 
removeVirtualNodes <- function(x, spec) {
  # check x is an antaresOutput object with elements nodes and links
  if (!is(x, "antaresOutput") || is.null(x$nodes) || is.null(x$links))
    stop("x has to be an 'antaresOutput' object with elements 'nodes' and 'links'")
  
  # Be sure that x$nodes and x$links are ordered by timeId
  setkey(x$nodes, node, timeId)
  setkey(x$links, link, timeId)
  
  vnodes <- c() # list of virtual nodes that need to be removed at the end
  
  linkList <- unique(x$links$link)
  
  for (s in spec) {
    # Make correction and complete the list of virtual nodes
    
    if (s$type == "pumped storage") {
      
      if (!is.null(x$nodes$BALANCE)) {
        
        for (tn in s$trueNodes) {
          for (vn in s$virtualNodes) {
            if ((l <- paste(tn, vn, sep=" - ")) %in% linkList) {
              x$nodes[J(tn)]$BALANCE <- x$nodes[J(tn)]$BALANCE - x$links[J(l)]$`FLOW LIN.` 
            } else if ((l <- paste(vn, tn, sep=" - ")) %in% linkList) {
              x$nodes[J(tn)]$BALANCE <- x$nodes[J(tn)]$BALANCE + x$links[J(l)]$`FLOW LIN.`
            }
          }
        }
        
      }
      
      vnodes <- union(vnodes, c(s$virtualNodes))
      
    }
    
  }
  
  # Remove all data about virtual nodes in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$node)) x[[n]] <- x[[n]][!node %in% vnodes]
    if (!is.null(x[[n]]$link)) x[[n]] <- x[[n]][!link %in% getLinks(vnodes)]
  }
  
  x
}

