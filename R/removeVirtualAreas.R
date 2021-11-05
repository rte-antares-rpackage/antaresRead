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
#'   storage/flexibility areas. Can also be a named list. Names are columns 
#'   to add and elements the virtual areas to group.
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
#' @param rowBal
#'   If \code{TRUE}, then BALANCE will be corrected by ROW. BAL:
#'   BALANCE := BALANCE - "ROW. BAL"
#' @param prodVars
#'   Virtual productions columns to add to real area. 
#'   Default to \code{getAlias("rmVA_production")}
#' @param costsVars
#'   If parameter \code{reassignCosts} is TRUE, affected columns.
#'   Default to \code{OV. COST}, \code{OP. COST}, \code{CO2 EMIS.} and \code{NP COST}
#' @param costsOn
#'   If parameter \code{reassignCosts} is TRUE, then the costs of the 
#'  virtual areas are reassigned to the real areas they are connected to.
#'  You can choose to reassigned production & storageFlexibility virtuals areas
#'  ("both", default), or only "production" or "storageFlexibility" virtuals areas
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
#'   \item Correct the balance of the real areas (and districts) by removing the flows
#'     to or from virtual areas.
#'   
#'   \item If parameter \code{reassignCosts} is TRUE, then the costs of the 
#'     virtual areas are reassigned to the real areas they are connected to. The
#'     default affected columns are \code{OV. COST}, \code{OP. COST}, \code{CO2 EMIS.}
#'     and \code{NP COST}. If a virtual area is connected to a single real area,
#'     all its costs are attributed to the real area. If it is connected to
#'     several real areas, then costs at a given time step are divided between
#'     them proportionally to the flows between them and the virtual area.
#'     An aggregation is done at the end to correct districts costs. 
#'   
#'   \item For each storage/flexibility area, a column named like the area is 
#'     created. It contains the values of the flow between the virtual area and 
#'     the real areas. This column is interpreted as a production of
#'     electricity: it is positive if the flow from the virtual area to the real
#'     area is positive and negative otherwise. If parameter \code{newCols} is
#'     \code{FALSE}, the values are added to the variable \code{PSP} and the 
#'     columns is removed.
#'     An aggregation is done at the end to add virtual storage/flexibility to districts. 
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
#'     An aggregation is done at the end to add virtual production to districts. 
#'   
#'   \item Finally, virtual areas and the links connected to them are removed
#'     from the data. 
#' }
#'   
#' The functions makes a few assumptions about the network. If they are
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
#' correctedData <- removeVirtualAreas(
#'     x = data, 
#'     storageFlexibility = c("psp in", "psp out"),
#'     production = "offshore"
#' )
#'                                     
#' correctedData_list <- removeVirtualAreas(
#'     x = data, 
#'     storageFlexibility = list(PSP = c("psp in", "psp out")),
#'     production = "offshore"
#' )
#'  
#'                                    
#' correctedData_details <- removeVirtualAreas(
#'     x = data, 
#'     storageFlexibility = list(PSP_IN = "psp in", PSP_OUT =  "psp out"),
#'     production = "offshore"
#' )
#'                                     
#' }
#' 
#' @export
#' 
removeVirtualAreas <- function(x, 
                               storageFlexibility = NULL, 
                               production = NULL, 
                               reassignCosts = FALSE, 
                               newCols = TRUE,
                               rowBal = TRUE, 
                               prodVars = getAlias("rmVA_production"), 
                               costsVars = c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"), 
                               costsOn = c("both", "storageFlexibility", "production")) {
  
  
  # check x is an antaresData object with elements areas and links
  if (!is(x, "antaresDataList") || is.null(x$areas) || is.null(x$links))
    stop("x has to be an 'antaresDataList' object with elements 'areas' and 'links'")
  
  if (is.null(storageFlexibility) & is.null(production))
    stop("At least one argument of 'storageFlexibility' and 'production' needs to be specified")
  
  if (!is.null(storageFlexibility))
  {
    if(is.list(storageFlexibility)){
      if(is.null(names(storageFlexibility))){
        stop("If using list for storageFlexibility, you have to use named list.")
      }
    }
    if (!any(unlist(storageFlexibility) %in% unique(x$areas$area))){
      warning("no one of you storageFlexibility areas are load in data")
    }
    if (is.list(storageFlexibility) && newCols){
      warning("`newCols` will be ignore for storageFlexibility. Use named list instead.")
    }
  }
  
  if (!is.null(production))
  {
    if (!any(production %in% unique(x$areas$area))){
      warning("no one of you production areas are load in data")
    }
  }
  
  costsOn <- match.arg(costsOn)
  
  opts <- simOptions(x)
  
  # Keep only virtual areas present in data. Note, that for storage/flexibility 
  # nodes, we do not need area data, but only links data, unless reassignCosts
  # is TRUE
  areaList <- as.character(unique(x$areas$area))
  production <- intersect(production, areaList)
  if (reassignCosts) {
    if(is.list(storageFlexibility)){
      storageFlexibility <- lapply(storageFlexibility, function(X) intersect(X, areaList))
    } else {
      storageFlexibility <- intersect(storageFlexibility, areaList)
    }
  }
  
  vareas <- c(unlist(storageFlexibility), production) # list of virtual areas that need to be removed at the end
  
  prodAreas <- x$areas[area %in% production]
  storageAreas <- x$areas[area %in% unlist(storageFlexibility)]
  
  by <- .get_by(x)
  byarea <- .get_by_area(x)
  bylink <- .get_by_link(x)
  
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
  linkList$connectedToVirtualArea <- linkList$to %in% unlist(storageFlexibility)
  
  connectedToHub <- linkList[, .(connectedToHub = all(connectedToVirtualArea)), 
                             by = area]
  
  
  if (any(connectedToHub$connectedToHub)) {
    
    veryVirtualAreas <- connectedToHub[connectedToHub == TRUE]$area
    
    # Remove very virtual areas from data
    x <- removeVirtualAreas(x, storageFlexibility = veryVirtualAreas)
    
    # Update parameters
    
    if(is.list(storageFlexibility)){
      storageFlexibility <- lapply(storageFlexibility, function(X){
        intersect(X, connectedToHub[connectedToHub == FALSE]$area)
      })
    } else {
      storageFlexibility <- intersect(storageFlexibility, connectedToHub[connectedToHub == FALSE]$area)
    }
    
    vareas <- c(unlist(storageFlexibility), production) 
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
                 by = "link", allow.cartesian = TRUE)
  
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
  if (!is.null(x$areas$BALANCE)) {
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
    colCostToCorrect <-  costsVars
    varCost <- intersect(names(x$areas), colCostToCorrect)
    
    if(costsOn == "both"){
      costs <- rbind(prodAreas, storageAreas)
    } else if(costsOn == "storageFlexibility"){
      costs <- storageAreas
    } else if(costsOn == "production"){
      costs <- prodAreas
    }

    costs <- costs[area %in% vareas, c(byarea, varCost), with = FALSE]
    
    # Add column "flow" to 'costs'
    flows[, area := varea]
    setkeyv(costs, byarea)
    setkeyv(flows, byarea)
    costs <- costs[flows, c(byarea, varCost, "flow", "rarea"), with = FALSE]
    
    # Compute the proportion of the cost to repercute on each real area
    costs[, c("totalFlow", "N") := list(sum(abs(flow)), .N), by = byarea]
    costs[, prop := ifelse(totalFlow == 0, 1 / N, abs(flow / totalFlow))]
    
    # Aggregate corrections by real area
    costs$area <- costs$rarea
    costs <- costs[, lapply(.SD, function(x) sum(x * prop)), by = byarea, .SDcols = c(varCost, "prop")]
    
    x$areas[, c(varCost) := lapply(mget(varCost), as.numeric)]
    setkeyv(costs, byarea)
    setkeyv(x$areas, byarea)
    x$areas[costs, 
            c(varCost) := as.data.table(mget(varCost)) + 
              as.data.table(mget(paste0("i.", varCost)))]
    x <- .merge_Col_Area_D(x, 
                           colMerge = colCostToCorrect,
                           opts = opts)
  }
  
  # Add a column for each storage/flexibility area
  if (length(storageFlexibility) > 0) {
    flows[, area := rarea]
    
    if (newCols & !is.list(storageFlexibility)) {
      
      
      # Create a new column for each virtual area
      formula <- sprintf("%s ~ varea", paste(byarea, collapse = " + "))
      
      tmp <- dcast(flows[varea %in% storageFlexibility, mget(c("varea", byarea, "flow"))], 
                   as.formula(formula), value.var = "flow")
      
      
      
      x$areas <- .mergeByRef(x$areas, tmp, on = byarea)
      
      # Replace NA values by zeros
      v <- storageFlexibility[vapply( # areas with at least one link
        storageFlexibility,
        FUN = function(x) length(getLinks(x)) > 0,
        FUN.VALUE = logical(1))]
      x$areas[, c(v) := lapply(mget(v), function(x) ifelse(is.na(x), 0, x))]
      # correct values for district
      x <- .merge_Col_Area_D(x, 
                             colMerge = v,
                             opts = opts)
      
    } else {
      # Add the virtual flows to column PSP. If column PSP does not exist, it is
      # created.
      
      # ####TEST CODE
      # #OLD
      # storageFlexibility = 'x_open_turb'
      # #NEW
      # storageFlexibility = list("mynowProd" = "x_open_turb")
      # 
      
      if(is.list(storageFlexibility)){
        
        for(i in 1:length(storageFlexibility)){
          new_name <- names(storageFlexibility)[[i]]
          new_storageFlexibility = storageFlexibility[[i]]
          if (is.null(x$areas[[new_name]])) {
            x$areas[, new_name := 0]
            setnames(x$areas, "new_name", new_name)
          }
          
          psp <- copy(flows[varea %in% new_storageFlexibility, 
                            corrPSP := sum(flow), 
                            by = c(byarea)])
          
          psp <- psp[varea %in% new_storageFlexibility]
          
          psp[, setdiff(names(psp), c(byarea, "corrPSP")) := NULL]
          
          .mergeByRef(x$areas, psp, on = byarea)
          
          x$areas[, `:=`(n_name = eval(parse(text = new_name)) + ifelse(is.na(corrPSP), 0, corrPSP), 
                         corrPSP = NULL
          )]
          
          x$areas[,  c(new_name) := NULL]
          
          setnames(x$areas, "n_name", new_name)
          
          # correct values for district
          x <- .merge_Col_Area_D(x, 
                                 colMerge = new_name, 
                                 allX = FALSE,
                                 opts = opts)
        }
        
      } else {
        
        if (is.null(x$areas$PSP)) x$areas[, PSP := 0]
        
        psp <- copy(flows[varea %in% storageFlexibility, 
                          corrPSP := sum(flow), 
                          by = c(byarea)])
        
        psp <- psp[varea %in% storageFlexibility]
        
        psp[, setdiff(names(psp), c(byarea, "corrPSP")) := NULL]
        
        
        .mergeByRef(x$areas, psp, on = byarea)
        x$areas[, `:=`(
          PSP = PSP + ifelse(is.na(corrPSP), 0, corrPSP), 
          corrPSP = NULL
        )]
        
        # correct values for district
        x <- .merge_Col_Area_D(x, 
                               colMerge = "PSP", 
                               allX = FALSE,
                               opts = opts)
      }
    }
  }
  
  # Aggregate production of production virtual areas and add columns for each 
  # type of production.
  if (length(production) > 0) {
    linkListProd <- flows[varea %in% production]
    
    # Add virtual productions columns to x$areas
    prodVars <- intersect(names(x$areas), prodVars)
    
    vars <- c(byarea, prodVars)
    
    virtualProd <- prodAreas[, vars, with = FALSE]
    
    # Remove columns containing only zeros
    for (v in prodVars) {
      if (all(virtualProd[[v]] == 0)) virtualProd[, c(v) := NULL]
    }
    prodVars <- prodVars[prodVars %in% names(virtualProd)]
    
    if(length(prodVars) > 0){
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
        for (i in prodVars){
          x$areas[, c(i) := mapply(sum, get(i), get(paste0(i, "_virtual")))]
        }
        x <- .merge_Col_Area_D(x, 
                               colMerge = prodVars,
                               opts = opts)
        x$areas[, c(v) := NULL]
      }else{
        x <- .merge_Col_Area_D(x, 
                               colMerge = v,
                               opts = opts)
      }
    }
  }
  
  # Put clusters of the virtual areas in the corresponding real areas
  # TODO we must rename production virtual areas to productionVirual 
  # cluster has a "production" column 
  productionVirual <- production
  if (!is.null(x$clusters)){
    if (length(unique(x$clusters[area %in% productionVirual, area])) > 0){
      linkListProdVirtual <- linkListProd[varea %in% productionVirual]
      linkListProdVirtual$area <- linkListProdVirtual$varea
      x$clusters <- merge(x$clusters, linkListProdVirtual[, mget(c(byarea, "rarea"))],
                          by = byarea, all.x = TRUE)
      x$clusters[!is.na(rarea), area := rarea]
      x$clusters[, rarea := NULL]
      idColsclusters <- .idCols(x$clusters)
      if(nrow(x$cluster[, c(idColsclusters), with = FALSE]) != nrow(unique(x$cluster[, c(idColsclusters), with = FALSE]))){
        var_names <- setdiff(colnames(x$clusters), idColsclusters)
        x$clusters <- x$cluster[, lapply(.SD, function(x) sum(x, na.rm = T)),
                                by = idColsclusters, .SDcols = var_names]
      }
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
    
    idColsLinks <- .idCols(x$links)
    
    pspCapacity <- merge(
      linkList[area %in% unlist(storageFlexibility), .(link, area = to, direction)],
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
  
  #correct district data 
  if (!is.null(x$districts) && length(storageFlexibility) > 0 && !is.null(x$areas$pumpingCapacity)){
    
    
    
    x <- .merge_Col_Area_D(x, 
                           colMerge = c("pumpingCapacity", "storageCapacity"),
                           opts = opts, allX = FALSE)
  }
  
  # correct balance district but only at the end, 
  # with the final x (after removing veryVirtualAreas) so we must keep all.y
  if (!is.null(x$areas$BALANCE)){
    x <- .merge_Col_Area_D(x, 
                           colMerge = "BALANCE",
                           opts = opts,
                           allX = FALSE)
  }
  
  # Store in attributes the name of the virtuals nodes
  if(is.list(storageFlexibility)){
    attr(x, "virtualNodes") <- list(
      storageFlexibility = setdiff(unique(c(attr(x, "virtualNodes")$storageFlexibility, names(storageFlexibility), unlist(storageFlexibility))), "PSP"),
      production = unique(c(attr(x, "virtualNodes")$production, production))
    )
  } else {
    attr(x, "virtualNodes") <- list(
      storageFlexibility = unique(c(attr(x, "virtualNodes")$storageFlexibility, storageFlexibility)),
      production = unique(c(attr(x, "virtualNodes")$production, production))
    )
  }
  
  
  if (attr(x, "synthesis")){
    colCostToCorrect <-  c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST")
    colMustChange <- c("BALANCE", colCostToCorrect, "virtualProd")
    warningMessage <- .tidymess(paste0("Colmuns : ", paste(colMustChange, collapse = ", "), " will 
                   be corrected but no statistical variables like BALANCE_min, BALANCE_max and 
                   BALANCE_std. If you want an accurate result, use removeVirtualAreas with 
                   detailed results and then use antaresProcessing::synthesize"))
    warning(warningMessage)
  }
  
  if (rowBal){
    # for R CMD Check 
    #  no visible binding for global variable 'ROW BAL.'
    `ROW BAL.` <- NULL
    if (!is.null(x$areas$`ROW BAL.`)){
      # edit BALANCE if ROW BAL is not always null
      if(min(unique(x$areas$`ROW BAL.`)) > 1){
        x$areas[, BALANCE := BALANCE - `ROW BAL.`, by = byarea]
        x$areas[, `ROW BAL.` := 0]
        x <- .merge_Col_Area_D(x, 
                               colMerge = c("BALANCE", "ROW BAL."),
                               opts = opts)
      }
    }
  }
  
  x
}
