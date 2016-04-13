#' Remove virtual nodes
#' 
#' This function removes virtual nodes from an \code{antaresOutput} object and
#' corrects the data for the real nodes. The \code{antaresOutput} object should
#' contain node and link data to function correctly. 
#' 
#' @param x
#'   An object of class \code{antaresOutput} with at least components 
#'   \code{nodes} and \code{links}
#' @param storageFlexibility
#'   A vector containing the names of the virtual storage/flexibility nodes
#' @param production
#'   A vector containing the names of the virtual production nodes
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
#' correctedData <- removeVirtualNodes(data, storageFlexibility = c("psp in", "psp out"))
#' }
#' 
#' @export
#' 
removeVirtualNodes <- function(x, storageFlexibility = NULL, production = NULL) {
  # check x is an antaresOutput object with elements nodes and links
  if (!is(x, "antaresOutput") || is.null(x$nodes) || is.null(x$links))
    stop("x has to be an 'antaresOutput' object with elements 'nodes' and 'links'")
  
  # Be sure that x$nodes and x$links are ordered by timeId
  setkey(x$nodes, node, timeId)
  setkey(x$links, link, timeId)
  
  vnodes <- c() # list of virtual nodes that need to be removed at the end
  
  linkList <- unique(x$links$link)
  nodeList <- unique(x$nodes$node)
  
  # Manage storage/flexbility nodes
  if (!is.null(storageFlexibility)) {
    
    # Analyse links to identify is there are hubs among virtual nodes
    
    for (vn in storageFlexibility) {
      
      # get all nodes linked to the virtual node
      links <- ldply(getLinks(vn), function(x) {
        xx <- strsplit(x, " - ")[[1]]
        if (xx[1] == vn) return (data.table(link = x, to = xx[2], direction = "out"))
        else return (data.table(link = x, to = xx[1], direction = "in"))
      })
      
      # Total flow of the virtual node. Used when a single virtual node is
      # connected to many real nodes in order to distribute costs between them
      totalFlow <- x$links[J(links$link), .(totalFlow = sum(abs(`FLOW LIN.`))), keyby = timeId]
      
      totalFlow <- totalFlow[totalFlow == 0, totalFlow := 1] # To avoid divide by 0 errors
      
      # Loop over real nodes connected to the virtual node
      for (i in 1:nrow(links)) {
        if (! links$to[i] %in% nodeList) next
        
        tn <- links$to[i]
        l <- links$link[i]
        dir <- ifelse(links$direction[i] == "in", -1, 1)
        
        flow <- dir * x$links[J(l)]$`FLOW LIN.`
        
        # Correct balance
        if (! is.null(x$nodes$BALANCE)) 
          x$nodes[J(tn)]$BALANCE <- x$nodes[J(tn)]$BALANCE + flow
        
        # Correct costs and CO2
        prop <- abs(flow / totalFlow$totalFlow)
        
        for (v in c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST")) {
          if (! is.null(x$nodes[[v]]))
            x$nodes[J(tn)][[v]] <- x$nodes[J(tn)][[v]] + prop * x$nodes[J(vn)][[v]]
        }
        
      }
    }
    
    vnodes <- union(vnodes, c(storageFlexibility))
    
  }
  
  # Remove all data about virtual nodes in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$node)) x[[n]] <- x[[n]][!node %in% vnodes]
    if (!is.null(x[[n]]$link)) x[[n]] <- x[[n]][!link %in% getLinks(vnodes)]
  }
  
  x
}

