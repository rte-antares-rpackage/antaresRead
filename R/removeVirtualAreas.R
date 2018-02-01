#Copyright © 2016 RTE Réseau de transport d’électricité

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
#' @param newCols
#'   If \code{TRUE}, new columns containing the production of the virtual
#'   areas are added. If FALSE their production is added to the production of
#'   the real areas they are connected to.
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
#'     area is positive and negative otherwise. If parameter \code{newCols} is
#'     \code{FALSE}, the values are added to the variable \code{PSP} and the 
#'     columns is removed.
#'     
#'   \item If the parameter \code{production} is specified, then the non null
#'     productions of the virtual areas are either added to the ones of the real 
#'     areas they are connected to if \code{newCols = FALSE} or put in new 
#'     columns if \code{newCols = TRUE}. In the second case the columns are 
#'     named \code{*_virtual} where "\code{*}" is a type of 
#'     production (wind, solar, nuclear, ...). Productions that are zero for
#'     all virtual areas are omited.
#'     If virtual production areas contains clusters then they will be move to the
#'     real area. 
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
                               reassignCosts = FALSE, newCols = TRUE) {
  
  # check x is an antaresData object with elements areas and links
  if (!is(x, "antaresDataList") || is.null(x$areas) || is.null(x$links))
    stop("x has to be an 'antaresDataList' object with elements 'areas' and 'links'")
  
  if (is.null(storageFlexibility) & is.null(production))
    stop("At least one argument of 'storageFlexibility' and 'production' needs to be specified")
  
  if(!is.null(storageFlexibility))
    {
      if(!any(storageFlexibility %in% unique(x$areas$area))){
        warning("no one of you storageFlexibility areas are load in data")
      }
  }
  
  
  if(!is.null(production))
  {
    if(!any(production %in% unique(x$areas$area))){
      warning("no one of you production areas are load in data")
    }
  }
  
  
  opts <- simOptions(x)
  
  # Keep only virtual areas present in data. Note, that for storage/flexibility 
  # nodes, we do not need area data, but only links data, unless reassignCosts
  # is TRUE
  areaList <- as.character(unique(x$areas$area))
  production <- intersect(production, areaList)
  if (reassignCosts) {
    storageFlexibility <- intersect(storageFlexibility, areaList)
  }
  vareas <- c(storageFlexibility, production) # list of virtual areas that need to be removed at the end
  
  prodAreas <- x$areas[area %in% production]
  storageAreas <- x$areas[area %in% storageFlexibility]
  
  # Aliases used for aggregation
  if (attr(x, "synthesis")) {
    by <- c("timeId")
  } else {
    by <- c("mcYear", "timeId")
  }
  bylink <- c("link", by)
  byarea <- c("area", by)
  
  # Table with the definition of the links
  linkList <- getLinks(vareas, namesOnly = FALSE, withDirection = TRUE)
  
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
                             by = area]
  
  
  if (any(connectedToHub$connectedToHub)) {
    
    veryVirtualAreas <- connectedToHub[connectedToHub == TRUE]$area
    
    # Remove very virtual areas from data
    x <- removeVirtualAreas(x, storageFlexibility = veryVirtualAreas)
    
    # Update parameters
    storageFlexibility <- intersect(storageFlexibility, connectedToHub[connectedToHub == FALSE]$area)
    vareas <- c(storageFlexibility, production) 
    linkList <- linkList[area %in% vareas]
    
    # Remove columns added for very virtual areas
    x$areas[, c(veryVirtualAreas) := NULL]
  }
  
  # Make a copy of x$areas with only real areas. This ensures that we do not modify
  # accidentally the input data by reference
  x$areas <- x$areas[!area %in% vareas]
  
  # Flows between virtual and real nodes.
  #
  # These flows are used two times:
  # 1 - to correct balance fo real areas
  # 2 - to compute the share of each real area when reassigning the costs of the
  #     virtual nodes
  flows <- merge(linkList, 
                 x$links[, c(bylink, "FLOW LIN."), with = FALSE], 
                 by = "link")
  
  flows[, `:=`(
    flow = `FLOW LIN.` * direction, # Change sign of flows when links are in wrong direction
    varea = area,                   # virtual area connected by the link
    rarea = to,                     # real area connected by the link
    area = to,                      # dummy variable used for merges
    "FLOW LIN." = NULL,
    direction = NULL,
    to = NULL
  )]
  
  # Correct balance
  if (! is.null(x$areas$BALANCE)) {
    x$areas[, BALANCE := as.numeric(BALANCE)]
    
    corrections <- flows[, .(correction = sum(flow)), keyby = byarea]
    
    .mergeByRef(x$areas, corrections, on = byarea, colsToAdd = "correction")
    x$areas[, `:=`(
      BALANCE = BALANCE + ifelse(is.na(correction), 0, correction),
      correction = NULL
    )]
  }
  
  # Correct costs and CO2
  if (reassignCosts) {
    varCost <- intersect(names(x$areas), c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"))
    
    costs <- rbind(prodAreas, storageAreas)
    costs <- costs[area %in% vareas, c(byarea, varCost), with = FALSE]
    
    # Add column "flow" to 'costs'
    flows[, area := varea]
    setkeyv(costs, byarea)
    setkeyv(flows, byarea)
    costs <- costs[flows, c(byarea, varCost, "flow", "rarea"), with = FALSE]
    
    # Compute the proportion of the cost to repercute on each real area
    costs[, c("totalFlow", "N") := list(sum(abs(flow)), .N), by = byarea]
    costs[, prop := ifelse(totalFlow == 0, 1/N, abs(flow / totalFlow))]
    
    # Aggregate corrections by real area
    costs$area <- costs$rarea
    costs <- costs[, lapply(.SD, function(x) sum(x * prop)), by = byarea, .SDcols = c(varCost, "prop")]
    
    x$areas[, c(varCost) := lapply(mget(varCost), as.numeric)]
    setkeyv(costs, byarea)
    setkeyv(x$areas, byarea)
    x$areas[costs, 
            c(varCost) := as.data.table(mget(varCost)) + 
                            as.data.table(mget(paste0("i.",varCost)))]
  }
  
  # Add a column for each storage/flexibility area
  if (length(storageFlexibility) > 0) {
    flows[, area := rarea]
    
    if (newCols) {
      # Create a new column for each virtual area
      formula <- sprintf("%s ~ varea", paste(byarea, collapse = " + "))
      
      tmp <- dcast(flows[varea %in% storageFlexibility, mget(c("varea", byarea, "flow"))], 
                   as.formula(formula), value.var = "flow")
    
      x$areas <- .mergeByRef(x$areas, tmp, on = byarea)
      
      # Replace NA values by zeros
      v <- storageFlexibility
      x$areas[, c(v) := lapply(mget(v), function(x) ifelse(is.na(x), 0, x))]
      
    } else {
      # Add the virtual flows to column PSP. If column PSP does not exist, it is
      # created.
      if (is.null(x$areas$PSP)) x$areas[, PSP := 0]
      
      psp <- copy(flows[varea %in% storageFlexibility, 
                   corrPSP := sum(flow), 
                   by = c(byarea)])
      
      psp<-psp[varea %in% storageFlexibility]
      
      psp[ , setdiff(names(psp), c(byarea, "corrPSP")) := NULL]
      .mergeByRef(x$areas, psp, on = byarea)
      x$areas[, `:=`(
        PSP = PSP + ifelse(is.na(corrPSP), 0, corrPSP), 
        corrPSP = NULL
      )]
    }
  }
  
  # Aggregate production of production virtual areas and add columns for each 
  # type of production.
  if (length(production) > 0) {
    .prodNodes <- production # Just to prevent name conflicts with columns of x$areas
    
    linkListProd <- flows[varea %in% production]
    
    # Add virtual productions columns to x$areas
    prodVars <- intersect(names(x$areas), c(pkgEnv$varAliases$generation$select, 
                                            pkgEnv$varAliases$netLoad$select, 
                                            "SPIL. ENRG"))
    prodVars <- prodVars[prodVars != "LOAD"]
    vars <- c(byarea, prodVars)
    
    virtualProd <- prodAreas[, vars, with = FALSE]
    
    # Remove columns containing only zeros
    for (v in prodVars) {
      if(all(virtualProd[[v]] == 0)) virtualProd[, c(v) := NULL]
    }
    prodVars <- prodVars[prodVars %in% names(virtualProd)]
    
    # Rename columns by appending "_virtual" to their names
    setnames(virtualProd, prodVars, paste0(prodVars, "_virtual"))
    
    # Merging with original data
    # /!\ Undesired results if multiple real areas connected to the same
    # virtual area.
    setnames(virtualProd, "area", "varea")
    linkListProd$area <- linkListProd$rarea
    virtualProd <- merge(virtualProd, 
                         linkListProd[, c("varea", byarea), with = FALSE], 
                         by = c("varea", by))
    virtualProd$varea <- NULL
    virtualProd <- virtualProd[, lapply(.SD, sum), by = byarea]
    
    .mergeByRef(x$areas, virtualProd, on = byarea)
    
    # Replace NA values by zeros
    v <- paste0(prodVars, "_virtual")
    x$areas[, c(v) := lapply(mget(v), function(x) ifelse(is.na(x), 0, x))]
    
    # Prod are integers
    x$areas[, c(v) := lapply(.SD, as.integer), .SDcols = c(v)]
    
    if (!newCols) {
      for(i in prodVars){
        x$areas[, c(i) := mapply(sum, get(i), get(paste0(i, "_virtual")))]
      }
      x$areas[, c(v) := NULL]
    }
  }
  
  # Put clusters of the virtual areas in the corresponding real areas
  #TODO we must rename production virtual areas to productionVirual 
  # cluster has a "production" column 
  productionVirual<-production
  if (!is.null(x$clusters)){
    if (length(unique(x$clusters[area %in% productionVirual, area])) > 0){
      linkListProdVirtual <- linkListProd[varea %in% productionVirual]
      linkListProdVirtual$area <- linkListProdVirtual$varea
      x$clusters <- merge(x$clusters, linkListProdVirtual[, mget(c(byarea, "rarea"))],
                          by = byarea, all.x = TRUE)
      x$clusters[!is.na(rarea), area := rarea]
      x$clusters[, rarea := NULL]
    }
  } 
  
  # Remove all data about virtual areas in x
  for (n in names(x)) {
    if (!is.null(x[[n]]$area)){
      x[[n]] <- x[[n]][!area %in% vareas]
    }
  }
  
  # Remove virtual links but if present keep the capacity of the links connected
  # to storage flexibility areas. These capacities are needed to compute 
  # upward and downward margins.
  if (length(storageFlexibility) > 0 && !is.null(x$links$transCapacityDirect)) {
    l <- linkList[area %in% storageFlexibility, link]
    idColsLinks <- .idCols(x$links)
    
    pspCapacity <- merge(
      linkList[area %in% storageFlexibility, .(link, area = to, direction)],
      x$links[, c(idColsLinks, "transCapacityDirect", 
                  "transCapacityIndirect"), with = FALSE],
      by = "link"
    )
    
    pspCapacity[direction == -1, `:=`(
      transCapacityDirect = transCapacityIndirect,
      transCapacityIndirect = transCapacityDirect
    )]
    
    # Users tend to use a transmission capacity of 1 instead of 0 to avoid warnings.
    # The following lines correct this.
    pspCapacity[transCapacityDirect == 1, transCapacityDirect := 0]
    pspCapacity[transCapacityIndirect == 1, transCapacityIndirect := 0]
    
    pspCapacity <- pspCapacity[, 
      .(transCapacityDirect = sum(transCapacityDirect),
        transCapacityIndirect = sum(transCapacityIndirect)),
      by = c(.idCols(x$areas))
    ]
    
    if (is.null(x$areas$storageCapacity)) {
      x$areas[, `:=`(
        storageCapacity = 0,
        pumpingCapacity = 0
      )]
    }
    
    .mergeByRef(
      x$areas,
      pspCapacity[, c(.idCols(x$areas), "transCapacityDirect", 
                      "transCapacityIndirect"), with = FALSE]
    )
    
    x$areas[, `:=`(
      pumpingCapacity = pumpingCapacity + ifelse(is.na(transCapacityIndirect), 0, transCapacityIndirect),
      storageCapacity = storageCapacity + ifelse(is.na(transCapacityDirect), 0, transCapacityDirect),
      transCapacityDirect = NULL,
      transCapacityIndirect = NULL
    )]
    
  }
  
  x$links <- x$links[!link %in% linkList$link]
  
  if(!is.null(x$districts) && length(storageFlexibility) > 0 && !is.null(x$areas$pumpingCapacity)){
    
    stoPumAreas<-x$areas[, .(pumpingCapacity, storageCapacity), by=c("timeId", "area")]
    stoPumDistricts<-.groupByDistrict(stoPumAreas, opts)
    
    #the columns pumpingCapacity doesnt exist when we trade very virtual area
    if(is.null(x$districts$pumpingCapacity)){
      x$districts<-merge(x$districts, stoPumDistricts, by=c("timeId", "district"))
    }else {
      #for simple virtual areas take into account the previous merge
      x$districts<-merge(x$districts, stoPumDistricts, all.x=TRUE, by=c("timeId", "district"))
      
      x$districts[, ':=' (
        pumpingCapacity=pumpingCapacity.x + pumpingCapacity.y , 
        storageCapacity=storageCapacity.x + storageCapacity.y
      )]
      
      x$districts[, c("pumpingCapacity.x", "pumpingCapacity.y", "storageCapacity.x" , "storageCapacity.y"
                      ):= NULL]
      
    }
  }
  
  # Store in attributes the name of the virtuals nodes
  attr(x, "virtualNodes") <- list(storageFlexibility = storageFlexibility,
                                  production = production)
  
  x
}

