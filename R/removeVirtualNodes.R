#' Remove virtual nodes
#' 
#' This function removes virtual nodes from an \code{antaresDataList} object and
#' corrects the data for the real nodes. The \code{antaresDataList} object 
#' should contain node and link data to function correctly.
#' 
#' @param x An object of class \code{antaresDataList} with at least components 
#'   \code{nodes} and \code{links}.
#' @param storageFlexibility A vector containing the names of the virtual 
#'   storage/flexibility nodes.
#' @param production A vector containing the names of the virtual production 
#'   nodes.
#' @param reassignCosts If TRUE, the production costs of the virtual nodes are 
#'   reallocated to the real nodes they are connected to. If the virtual nodes 
#'   are connected to a virtual hub, their costs are first reallocated to the 
#'   hub and then the costs of the hub are reallocated to the real nodes.
#'   
#' @inheritParams readAntares
#'   
#' @return 
#' An \code{antaresDataList} object in which virtual nodes have been removed and
#' data of the real has been corrected. See details for an explanation of the
#' corrections.
#'   
#' @details 
#' Two types of virtual nodes have been defined corresponding to different types
#' of modeling in Antares and different types of post-treatment to do:
#'   
#' \itemize{ 
#'   \item Flexibility/storage nodes are nodes created to model 
#'     pumping unit or any other flexibility that behave as a storage. For those 
#'     virtual nodes, the important results are flows on the links. 
#'   \item Production nodes are nodes created to isolate some generation from 
#'     the “real” nodes. They can be isolate for several reasons: to distinguish 
#'     time-series (for example wind onshore/offshore), to select some specific 
#'     unit to participate to day-ahead reserve, etc.
#' }
#'   
#' \code{removeVirtualNodes} performs different corrections:
#'   
#' \itemize{ 
#'   \item Correct the balance of the real nodes by removing the flows
#'     to or from virtual nodes.
#'   
#'   \item If parameter \code{reassignCosts} is TRUE, then the costs of the 
#'     virtual nodes are reassigned to the real nodes they are connected to. The
#'     affected columns are \code{OV. COST}, \code{OP. COST}, \code{CO2 EMIS.}
#'     and \code{NP COST}. If a virtual node is connected to a single real node,
#'     all its costs are attributed to the real node. If it is connected to
#'     several real nodes, then costs at a given time step are divided between
#'     them proportionally to the flows between them and the virtual node.
#'   
#'   \item For each storage/flexibility node, a column named like the node is 
#'     created. It contains the values of the flow between the virtual node and 
#'     the real nodes. This column is interpreted as a production of
#'     electricity: it is positive if the flow from the virtual node to the real
#'     node is positive and negative otherwise.
#'   
#'   \item If the parameter \code{production} is specified, the functions
#'     creates new columns names \code{*_virtual} where "\code{*}" is a type of 
#'     production (wind, solar, nuclear, ...). For a given node, these columns 
#'     contain the production of the virtual production nodes connected to it.
#'     If a column contains only zeros, then it is removed in the returned
#'     object.
#'   
#'   \item Finally, virtual nodes and the links connected to them are removed
#'     from the data. 
#' }
#'   
#' The functions makes a few assumptions about the network. if they are
#' violated it will not act correctly: 
#'   
#' \itemize{ 
#'   \item storage/flexibility
#'     nodes can be connected to other storage/flexibility nodes (hubs), but at 
#'     least one of them is connected to a real node. That means that there is
#'     no group of virtual nodes disconnected from the real network. If such a
#'     group exists, you can either remove them manually or simply not import
#'     them.
#'   \item production nodes are connected to one and only one real node. They
#'     cannot be connected to virtual nodes. But a real node may by connected to
#'     several production nodes.
#' }
#'   
#' @examples 
#' \dontrun{
#' 
#' # Assume we have a network with two virtual nodes acting as pump storage and
#' # a node representing offshore production
#' #
#' #  offshore
#' #     |
#' # real node - psp in
#' #           \
#' #             psp out
#' #
#' 
#' data <- readAntares(nodes="all", links="all")
#' 
#' # Remove pump storage virtual nodes
#' 
#' correctedData <- removeVirtualNodes(data, 
#'                                     storageFlexibility = c("psp in", "psp out"),
#'                                     production = "offshore")
#' }
#' 
#' @export
#' 
removeVirtualNodes <- function(x, storageFlexibility = NULL, production = NULL, 
                               reassignCosts = FALSE) {
  
  opts <- simOptions(x)
  
  # check x is an antaresData object with elements nodes and links
  if (!is(x, "antaresDataList") || is.null(x$nodes) || is.null(x$links))
    stop("x has to be an 'antaresDataList' object with elements 'nodes' and 'links'")
  
  nodeList <- unique(x$nodes$node)
  
  vnodes <- c(storageFlexibility, production) # list of virtual nodes that need to be removed at the end
  if (is.null(vnodes)) stop("At least one argument of 'storageFlexibility' and 'production' needs to be specified")
  
  if (attr(x, "synthesis")) {
    
    by <- c("timeId")
  } else {
    by <- c("mcYear", "timeId")
  }
  
  bylink <- c("link", by)
  bynode <- c("node", by)
  setkeyv(x$nodes, bynode)
  
  # Create a table containing all links that connect virtual nodes to other nodes
  linkList <- ldply(vnodes, function(vn) {
    ldply(getLinks(vn, opts=opts, regexpSelect = FALSE), function(x) {
      xx <- strsplit(x, " - ")[[1]]
      if (xx[1] == vn) return (data.table(link = x, from = vn, to = xx[2], direction = "out"))
      else return (data.table(link = x, from = vn, to = xx[1], direction = "in"))
    })
  })
  
  linkList <- data.table(linkList)
  
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
    
    # Remove columns added for very virtual nodes
    for (v in veryVirtualNodes) x$nodes[[v]] <- NULL
  }
  
  linkList <- merge(linkList, x$links[, mget(c(bylink, "FLOW LIN."))], by = "link")
  
  # Some flows may be inversed depending on how the links are created. Correct this
  linkList$flow <- linkList[, `FLOW LIN.` * ifelse(direction == "in", -1, 1)]
  
  # Correct balance
  if (! is.null(x$nodes$BALANCE)) {
    x$nodes$BALANCE <- as.numeric(x$nodes$BALANCE)
    linkList$node <- linkList$to
    corrections <- linkList[, .(correction = sum(flow)), keyby = mget(bynode)]
    x$nodes <- merge(x$nodes, corrections, by = bynode, all.x = TRUE)
    x$nodes[!is.na(correction), BALANCE := BALANCE + correction]
    x$nodes[, correction := NULL]
  }
  
  # Correct costs and CO2
  if (reassignCosts) {
    varCost <- intersect(names(x$nodes), c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"))
    
    costs <- x$nodes[node %in% vnodes, mget(c(bynode, varCost))]
    linkList$node <- linkList$from
    costs <- merge(linkList[, mget(c(bynode, "to", "flow"))], costs, by = bynode)
    linkList$node <- NULL
    
    # Compute the proportion of the cost to repercute on each real node
    costs[, totalFlow := sum(abs(flow)), by = mget(bynode)]
    costs$prop <- ifelse(costs$totalFlow == 0, 1, abs(costs$flow / costs$totalFlow))
    
    # Aggregate corrections by real node
    costs$node <- costs$to
    costs <- costs[, lapply(.SD, function(x) sum(x * prop)), by = bynode, .SDcols = c(varCost, "prop")]
    
    for (v in varCost) {
      x$nodes[[v]] <- as.numeric(x$nodes[[v]])
      x$nodes[costs[, mget(bynode)]][[v]] <- x$nodes[costs[, mget(bynode)]][[v]] + costs[[v]]
    }
  }
  
  # Add a column for each storage/flexibility node
  if (!is.null(storageFlexibility)) {
    linkList$node <- linkList$to
    
    formula <- sprintf("%s ~ from", paste(bynode, collapse = " + "))
    
    tmp <- dcast(linkList[from %in% storageFlexibility, mget(c("from", bynode, "flow"))], 
                 as.formula(formula), value.var = "flow")
    
    x$nodes <- merge(x$nodes, tmp, by = bynode, all.x = TRUE)
    
    for (v in storageFlexibility) {
      x$nodes[[v]][is.na(x$nodes[[v]])] <- 0
    }
  }
  
  # Aggregate production of production virtual nodes and add columns for each 
  # type of production.
  if (!is.null(production)) {
    
    linkListProd <- linkList[from %in% production]
    
    # Add virtual productions columns to x$nodes
    prodVars <- intersect(names(x$nodes), c(pkgEnv$varAliases$generation, pkgEnv$varAliases$`net load`, "SPIL. ENRG"))
    prodVars <- prodVars[prodVars != "LOAD"]
    vars <- c(bynode, prodVars)
    
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
    linkListProd$node <- linkListProd$to
    virtualProd <- merge(virtualProd, 
                         linkListProd[, mget(c("from", bynode))], 
                         by = c("from", by))
    virtualProd$from <- NULL
    virtualProd <- virtualProd[, lapply(.SD, sum), by = mget(bynode)]
    x$nodes <- merge(x$nodes, virtualProd, by = bynode, all.x = TRUE)
    
    for (v in paste0(prodVars, "_virtual")) {
      x$nodes[[v]][is.na(x$nodes[[v]])] <- 0
    }
  }
  
  # Put clusters of the virtual nodes in the corresponding real ndoes
  if (!is.null(x$clusters) & !is.null(x$production)) {
    linkListProd <- linkList[from %in% production]
    linkListProd$node <- linkListProd$from
    x$clusters <- merge(x$clusters, linkListProd[, mget(c(bynode, "to"))],
                        by = bynode, all.x = TRUE)
    x$clusters[!is.na(to), node := to]
    x$clusters$to <- NULL
  }
  
  # Remove all data about virtual nodes in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$node)) x[[n]] <- x[[n]][!node %in% vnodes]
    if (!is.null(x[[n]]$link)) x[[n]] <- x[[n]][!link %in% getLinks(vnodes)]
  }
  
  x
}

