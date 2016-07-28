#' Remove virtual areas
#' 
#' This function removes virtual areas from an \code{antaresDataList} object and
#' corrects the data for the real areas. The \code{antaresDataList} object 
#' should contain area and link data to function correctly.
#' 
#' @param x An object of class \code{antaresDataList} with at least components 
#'   \code{areas} and \code{links}.
#' @param storageFlexibility A vector containing the names of the virtual 
#'   storage/flexibility areas.
#' @param production A vector containing the names of the virtual production 
#'   areas.
#' @param reassignCosts If TRUE, the production costs of the virtual areas are 
#'   reallocated to the real areas they are connected to. If the virtual areas 
#'   are connected to a virtual hub, their costs are first reallocated to the 
#'   hub and then the costs of the hub are reallocated to the real areas.
#'   
#' @inheritParams readAntares
#'   
#' @return 
#' An \code{antaresDataList} object in which virtual areas have been removed and
#' data of the real has been corrected. See details for an explanation of the
#' corrections.
#'   
#' @details 
#' Two types of virtual areas have been defined corresponding to different types
#' of modeling in Antares and different types of post-treatment to do:
#'   
#' \itemize{ 
#'   \item Flexibility/storage areas are areas created to model 
#'     pumping unit or any other flexibility that behave as a storage. For those 
#'     virtual areas, the important results are flows on the links. 
#'   \item Production areas are areas created to isolate some generation from 
#'     the "real" areas. They can be isolate for several reasons: to distinguish 
#'     time-series (for example wind onshore/offshore), to select some specific 
#'     unit to participate to day-ahead reserve, etc.
#' }
#'   
#' \code{removeVirtualAreas} performs different corrections:
#'   
#' \itemize{ 
#'   \item Correct the balance of the real areas by removing the flows
#'     to or from virtual areas.
#'   
#'   \item If parameter \code{reassignCosts} is TRUE, then the costs of the 
#'     virtual areas are reassigned to the real areas they are connected to. The
#'     affected columns are \code{OV. COST}, \code{OP. COST}, \code{CO2 EMIS.}
#'     and \code{NP COST}. If a virtual area is connected to a single real area,
#'     all its costs are attributed to the real area. If it is connected to
#'     several real areas, then costs at a given time step are divided between
#'     them proportionally to the flows between them and the virtual area.
#'   
#'   \item For each storage/flexibility area, a column named like the area is 
#'     created. It contains the values of the flow between the virtual area and 
#'     the real areas. This column is interpreted as a production of
#'     electricity: it is positive if the flow from the virtual area to the real
#'     area is positive and negative otherwise.
#'   
#'   \item If the parameter \code{production} is specified, the functions
#'     creates new columns names \code{*_virtual} where "\code{*}" is a type of 
#'     production (wind, solar, nuclear, ...). For a given area, these columns 
#'     contain the production of the virtual production areas connected to it.
#'     If a column contains only zeros, then it is removed in the returned
#'     object.
#'   
#'   \item Finally, virtual areas and the links connected to them are removed
#'     from the data. 
#' }
#'   
#' The functions makes a few assumptions about the network. if they are
#' violated it will not act correctly: 
#'   
#' \itemize{ 
#'   \item storage/flexibility
#'     areas can be connected to other storage/flexibility areas (hubs), but at 
#'     least one of them is connected to a real area. That means that there is
#'     no group of virtual areas disconnected from the real network. If such a
#'     group exists, you can either remove them manually or simply not import
#'     them.
#'   \item production areas are connected to one and only one real area. They
#'     cannot be connected to virtual areas. But a real area may by connected to
#'     several production areas.
#' }
#'   
#' @examples 
#' \dontrun{
#' 
#' # Assume we have a network with two virtual areas acting as pump storage and
#' # an area representing offshore production
#' #
#' #  offshore
#' #     |
#' # real area - psp in
#' #           \
#' #             psp out
#' #
#' 
#' data <- readAntares(areas="all", links="all")
#' 
#' # Remove pump storage virtual areas
#' 
#' correctedData <- removeVirtualAreas(data, 
#'                                     storageFlexibility = c("psp in", "psp out"),
#'                                     production = "offshore")
#' }
#' 
#' @export
#' 
removeVirtualAreas <- function(x, storageFlexibility = NULL, production = NULL, 
                               reassignCosts = FALSE) {
  
  opts <- simOptions(x)
  
  # check x is an antaresData object with elements areas and links
  if (!is(x, "antaresDataList") || is.null(x$areas) || is.null(x$links))
    stop("x has to be an 'antaresDataList' object with elements 'areas' and 'links'")
  
  areaList <- unique(x$areas$area)
  
  vareas <- c(storageFlexibility, production) # list of virtual areas that need to be removed at the end
  if (is.null(vareas)) stop("At least one argument of 'storageFlexibility' and 'production' needs to be specified")
  
  if (attr(x, "synthesis")) {
    
    by <- c("timeId")
  } else {
    by <- c("mcYear", "timeId")
  }
  
  bylink <- c("link", by)
  byarea <- c("area", by)
  setkeyv(x$areas, byarea)
  
  # Create a table containing all links that connect virtual areas to other areas
  linkList <- ldply(vareas, function(vn) {
    ldply(getLinks(vn, opts=opts), function(x) {
      xx <- strsplit(x, " - ")[[1]]
      if (xx[1] == vn) return (data.table(link = x, from = vn, to = xx[2], direction = "out"))
      else return (data.table(link = x, from = vn, to = xx[1], direction = "in"))
    })
  })
  
  linkList <- data.table(linkList)
  
  # Treatment of hubs:
  #
  # If a virtual area is only connected to virtual areas then it should be a
  # "very virtual" area and the areas it is connected to should be hubs. 
  # (by assumption there are only two levels of virtual areas)
  # In such case we remove the "very virtual" areas by using removeVirtualAreas
  # and treating the hubs as real areas. This way, the costs of the very
  # virtual areas are agregated in the hubs.
  # Finally we run treat the hubs as normal virtual areas and continue the
  # execution of the function.
  linkList$connectedToVirtualArea <- linkList$to %in% storageFlexibility
  
  connectedToHub <- linkList[, .(connectedToHub = all(connectedToVirtualArea)), 
                             by = from]
  
  if (any(connectedToHub$connectedToHub)) {
    
    veryVirtualAreas <- connectedToHub[connectedToHub == TRUE]$from
    
    # Remove very virtual areas from data
    x <- removeVirtualAreas(x, storageFlexibility = veryVirtualAreas)
    
    # Update parameters
    storageFlexibility <- intersect(storageFlexibility, connectedToHub[connectedToHub == FALSE]$from)
    vareas <- c(storageFlexibility, production) 
    linkList <- linkList[from %in% vareas]
    
    # Remove columns added for very virtual areas
    for (v in veryVirtualAreas) x$areas[[v]] <- NULL
  }
  
  linkList <- merge(linkList, x$links[, mget(c(bylink, "FLOW LIN."))], by = "link")
  
  # Some flows may be inversed depending on how the links are created. Correct this
  linkList$flow <- linkList[, `FLOW LIN.` * ifelse(direction == "in", -1, 1)]
  
  # Correct balance
  if (! is.null(x$areas$BALANCE)) {
    x$areas$BALANCE <- as.numeric(x$areas$BALANCE)
    linkList$area <- linkList$to
    corrections <- linkList[, .(correction = sum(flow)), keyby = mget(byarea)]
    x$areas <- merge(x$areas, corrections, by = byarea, all.x = TRUE)
    x$areas[!is.na(correction), BALANCE := BALANCE + correction]
    x$areas[, correction := NULL]
  }
  
  # Correct costs and CO2
  if (reassignCosts) {
    varCost <- intersect(names(x$areas), c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"))
    
    costs <- x$areas[area %in% vareas, mget(c(byarea, varCost))]
    linkList$area <- linkList$from
    costs <- merge(linkList[, mget(c(byarea, "to", "flow"))], costs, by = byarea)
    linkList$area <- NULL
    
    # Compute the proportion of the cost to repercute on each real area
    costs[, c("totalFlow", "N") := list(sum(abs(flow)), .N), by = byarea]
    costs$prop <- ifelse(costs$totalFlow == 0, 1/costs$N, abs(costs$flow / costs$totalFlow))
    
    # Aggregate corrections by real area
    costs$area <- costs$to
    costs <- costs[, lapply(.SD, function(x) sum(x * prop)), by = byarea, .SDcols = c(varCost, "prop")]
    
    x$areas[, `:=`(c(varCost), lapply(mget(varCost), as.numeric))]
    x$areas[costs[, mget(byarea)], 
            `:=`(c(varCost), as.data.table(mget(varCost)) + costs[, mget(varCost)])]
  }
  
  # Add a column for each storage/flexibility area
  if (!is.null(storageFlexibility)) {
    linkList$area <- linkList$to
    
    formula <- sprintf("%s ~ from", paste(byarea, collapse = " + "))
    
    tmp <- dcast(linkList[from %in% storageFlexibility, mget(c("from", byarea, "flow"))], 
                 as.formula(formula), value.var = "flow")
    
    x$areas <- merge(x$areas, tmp, by = byarea, all.x = TRUE)
    
    for (v in storageFlexibility) {
      x$areas[[v]][is.na(x$areas[[v]])] <- 0
    }
  }
  
  # Aggregate production of production virtual areas and add columns for each 
  # type of production.
  if (!is.null(production)) {
    .prodNodes <- production # Just to prevent name conflicts with columns of x$areas
    
    linkListProd <- linkList[from %in% production]
    
    # Add virtual productions columns to x$areas
    prodVars <- intersect(names(x$areas), c(pkgEnv$varAliases$generation, pkgEnv$varAliases$`net load`, "SPIL. ENRG"))
    prodVars <- prodVars[prodVars != "LOAD"]
    vars <- c(byarea, prodVars)
    
    virtualProd <- x$areas[area %in% .prodNodes, mget(vars)]
    
    # Remove columns containing only zeros
    for (v in prodVars) {
      if(all(virtualProd[[v]] == 0)) virtualProd[[v]] <- NULL
    }
    prodVars <- prodVars[prodVars %in% names(virtualProd)]
    
    # Rename columns by appending "_virtual" to their names
    setnames(virtualProd, prodVars, paste0(prodVars, "_virtual"))
    
    # Merging with original data
    # /!\ Undesired results if multiple real areas connected to the same
    # virtual area.
    setnames(virtualProd, "area", "from")
    linkListProd$area <- linkListProd$to
    virtualProd <- merge(virtualProd, 
                         linkListProd[, mget(c("from", byarea))], 
                         by = c("from", by))
    virtualProd$from <- NULL
    virtualProd <- virtualProd[, lapply(.SD, sum), by = mget(byarea)]
    x$areas <- merge(x$areas, virtualProd, by = byarea, all.x = TRUE)
    
    for (v in paste0(prodVars, "_virtual")) {
      x$areas[[v]][is.na(x$areas[[v]])] <- 0
    }
  }
  
  # Put clusters of the virtual areas in the corresponding real areas
  if (!is.null(x$clusters) & !is.null(x$production)) {
    linkListProd <- linkList[from %in% production]
    linkListProd$area <- linkListProd$from
    x$clusters <- merge(x$clusters, linkListProd[, mget(c(byarea, "to"))],
                        by = byarea, all.x = TRUE)
    x$clusters[!is.na(to), area := to]
    x$clusters$to <- NULL
  }
  
  # Remove all data about virtual areas in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$area)) x[[n]] <- x[[n]][!area %in% vareas]
  }
  
  # Keep in attributes the name of the virtuals nodes
  attr(x, "virtualNodes") <- list(storageFlexibility = storageFlexibility,
                                  production = production)
  
  x
}

