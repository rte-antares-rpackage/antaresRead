#' Remove virtual nodes
#' 
#' This function removes virtual nodes from an \code{antaresOutput} object and
#' corrects the data for the real nodes. The \code{antaresOutput} object should
#' contain node and link data to function correctly. 
#' 
#' @param x
#'   An object of class \code{antaresOutput} with at least components 
#'   \code{nodes} and \code{links}.
#' @param storageFlexibility
#'   A vector containing the names of the virtual storage/flexibility nodes.
#' @param production
#'   A vector containing the names of the virtual production nodes.
#' @param reassignCosts
#'   If TRUE, the production costs of the virtual nodes are reallocated to the
#'   real nodes they are connected to.If the virtual nodes are connected to a 
#'   virtual hub, their costs are first reallocated to the hub and then the 
#'   costs of the hub are reallocated to the real nodes.
#'   
#' @inheritParams readAntares
#'   
#' @return 
#' an \code{antaresOutput object} in which virtual nodes have been removed and 
#' data of the real has been corrected. See details for an explanation of the
#' corrections.
#' 
#' @details 
#' [Explain what is a storage/flexibility node and a production node]
#' 
#' [Explain the corrections the function makes]
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
removeVirtualNodes <- function(x, storageFlexibility = NULL, production = NULL, 
                               reassignCosts = TRUE, opts = getOption("antares")) {
  # check x is an antaresOutput object with elements nodes and links
  if (!is(x, "antaresOutput") || is.null(x$nodes) || is.null(x$links))
    stop("x has to be an 'antaresOutput' object with elements 'nodes' and 'links'")
  
  nodeList <- unique(x$nodes$node)
  
  vnodes <- c(storageFlexibility, production) # list of virtual nodes that need to be removed at the end
  if (is.null(vnodes)) stop("At least one argument of 'storageFlexibility' and 'production' needs to be specified")
  
  # Create a table containing all links that connect virtual nodes to other nodes
  linkList <- ldply(vnodes, function(vn) {
    ldply(getLinks(vn, opts=opts), function(x) {
      xx <- strsplit(x, " - ")[[1]]
      if (xx[1] == vn) return (data.table(link = x, from = vn, to = xx[2], direction = "out"))
      else return (data.table(link = x, from = vn, to = xx[1], direction = "in"))
    })
  })
  
  linkList <- data.table(linkList)
  
  linkList <- merge(linkList, x$links[, .(link, timeId, `FLOW LIN.`)], by = "link")
  
  # Some flows may be inversed depending on how the links are created. Correct this
  linkList$flow <- linkList[, `FLOW LIN.` * ifelse(direction == "in", -1, 1)]
  
  # Treatment of hubs:
  #
  # If a virtual node is only connected to virtual nodes then it should be a
  # "very virtual" node and the nodes it is connected to should be hubs. 
  # (by assumption there are only two levels of virtual nodes)
  # In such case we remove the "very virtual" nodes by using removeVirtualNodes
  # and treating the hubs as real nodes. This way, the costs of the very
  # virtual nodes are agregated in the hubs.
  # Finally we run treat the hubs as normal virtual nodes and continue the
  # execution of the function.
  linkList$connectedToVirtualNode <- linkList$to %in% storageFlexibility
  
  connectedToHub <- linkList[, .(connectedToHub = all(connectedToVirtualNode)), 
                             by = from]
  
  if (any(connectedToHub$connectedToHub)) {
    
    veryVirtualNodes <- connectedToHub[connectedToHub == TRUE]$from
    
    # Remove very virtual nodes from data
    x <- removeVirtualNodes(x, storageFlexibility = veryVirtualNodes)
    
    # Update parameters
    storageFlexibility <- connectedToHub[connectedToHub == FALSE]$from
    vnodes <- c(storageFlexibility, production) 
    linkList <- linkList[from %in% vnodes]
  }
  
  # Correct balance
  if (! is.null(x$nodes$BALANCE)) {
    corrections <- linkList[, .(correction = sum(flow)), keyby = .(node = to, timeId)]
    x$nodes <- merge(x$nodes, corrections, by = c("node", "timeId"), all.x = TRUE)
    x$nodes[!is.na(correction), BALANCE := BALANCE + correction]
    x$nodes$correction <- NULL
  }
  
  # Correct costs and CO2
  if (reassignCosts) {
    varCost <- intersect(names(x$nodes), c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"))
    
    costs <- x$nodes[node %in% vnodes, mget(c("node", "timeId", varCost))]
    setnames(costs, "node", "from")
    costs <- merge(linkList[, .(from, to, timeId, flow)], costs, by = c("from", "timeId"))
    
    # Compute the proportion of the cost to repercute on each real node
    costs[, totalFlow := max(1, sum(abs(flow))), by = .(from, timeId)]
    costs$prop <- abs(costs$flow / costs$totalFlow)
    
    setkey(x$nodes, node, timeId)
    for (v in varCost) {
      x$nodes[costs[,.(node, timeId)]][[v]] <- x$nodes[costs[,.(node, timeId)]][[v]] + costs$prop * costs[[v]]
    }
  }
  
  # Add a column for each storage/flexibility node
  if (!is.null(storageFlexibility)) {
    tmp <- dcast(linkList[from %in% storageFlexibility, .(from, node = to, timeId, flow)], 
                 node + timeId ~ from, value.var = "flow")
    
    x$nodes <- merge(x$nodes, tmp, by = c("node", "timeId"), all.x = TRUE)
    
    for (v in storageFlexibility) {
      x$nodes[[v]][is.na(x$nodes[[v]])] <- 0
    }
  }
  
  # Aggregate production of production virtual nodes and add columns for each 
  # type of production.
  if (!is.null(production)) {
    
    linkListProd <- linkList[from %in% production]
    
    # Add virtual productions columns to x$nodes
    prodVars <- intersect(names(x$nodes), c(pkgEnv$varAliases$generation, "SPIL. ENRG"))
    vars <- c("node", "timeId", prodVars)
    
    virtualProd <- x$nodes[node %in% production, mget(vars)]
    
    # Remove columns containing only zeros
    for (v in prodVars) {
      if(all(virtualProd[[v]] == 0)) virtualProd[[v]] <- NULL
    }
    prodVars <- prodVars[prodVars %in% names(virtualProd)]
    
    # Rename columns by appending "_virtual" to their names
    setnames(virtualProd, prodVars, paste0(prodVars, "_virtual"))
    
    # Merging with original data
    # /!\ Undesired results if multiple real nodes connected to the same
    # virtual node.
    setnames(virtualProd, "node", "from")
    virtualProd <- merge(virtualProd, 
                         linkListProd[, .(from, node = to, timeId)], 
                         by = c("from", "timeId"))
    virtualProd$from <- NULL
    virtualProd <- virtualProd[, lapply(.SD, sum), by = .(node, timeId)]
    x$nodes <- merge(x$nodes, virtualProd, by = c("node", "timeId"), all.x = TRUE)
    
    for (v in paste0(prodVars, "_virtual")) {
      x$nodes[[v]][is.na(x$nodes[[v]])] <- 0
    }
  }
  
  # Remove all data about virtual nodes in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$node)) x[[n]] <- x[[n]][!node %in% vnodes]
    if (!is.null(x[[n]]$link)) x[[n]] <- x[[n]][!link %in% getLinks(vnodes)]
  }
  
  x
}

