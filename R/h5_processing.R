#' Add columns to an antares .h5 file
#' 
#' @description In this version only hourly data can be enriched. 
#'
#' @param opts \code{simOptions} obtain wich [antaresRead]{setSimulationPath}
#' @param mcY  \code{character}, "mcInd" or "mcAll".
#' @param addDownwardMargin \code{boolean} refer to [antaresProcessing]{addDownwardMargin}
#' @param addUpwardMargin \code{boolean} refer to [antaresProcessing]{addUpwardMargin}
#' @param addExportAndImport \code{boolean} refer to [antaresProcessing]{addExportAndImport}
#' @param addLoadFactorLink \code{boolean} refer to [antaresProcessing]{addLoadFactorLink}
#' @param externalDependency \code{boolean} refer to [antaresProcessing]{externalDependency}
#' @param loadFactor \code{boolean} refer to [antaresProcessing]{loadFactor}
#' @param modulation \code{boolean} refer to [antaresProcessing]{modulation}
#' @param netLoadRamp \code{boolean} refer to [antaresProcessing]{netLoadRamp}
#' @param surplus \code{boolean} refer to [antaresProcessing]{surplus}
#' @param surplusClusters \code{boolean} refer to [antaresProcessing]{surplusClusters}
#' @param evalAreas \code{list}, list of operation to evaluate in areas data
#' @param evalLinks \code{list}, list of operation to evaluate in links data
#' @param evalClusters \code{list}, list of operation to evaluate in clusters data
#' @param evalDistricts \code{list}, list of operation to evaluate in districts data
#' @param nThreads \code{numeric}, nThreads to use
#' 
#' @examples
#' 
#' \dontrun{
#' addStraitments(opts = opts,  mcY = "mcInd",
#'                addDownwardMargin = TRUE,
#'                addUpwardMargin = TRUE,
#'                addExportAndImport = TRUE,
#'                addLoadFactorLink = TRUE,
#'                externalDependency = TRUE,
#'                loadFactor = TRUE,
#'                modulation = TRUE,
#'                netLoadRamp = TRUE,
#'                surplus = TRUE,
#'                surplusClusters = TRUE,
#'                evalAreas = list(Tota = "`H. STOR` + `MISC. DTG`",
#'                                 Tota2 = "`NODU` + `NP COST` + 1"),
#'                evalLinks = list(),
#'                evalClusters = list(),
#'                evalDistricts = list()
#'                )
#' }
#' 
#' @export
addStraitments <- function(opts,
                           mcY = c("mcInd", "mcAll"),
                           addDownwardMargin = FALSE,
                           addUpwardMargin = FALSE,
                           addExportAndImport = FALSE,
                           addLoadFactorLink = FALSE,
                           externalDependency = FALSE,
                           loadFactor = FALSE,
                           modulation = FALSE,
                           netLoadRamp = FALSE,
                           surplus = FALSE,
                           surplusClusters = FALSE,
                           evalAreas = list(),
                           evalLinks = list(),
                           evalClusters = list(),
                           evalDistricts = list(), nThreads = 1){
  
  mcY <- match.arg(mcY)
  allStraitments <- list(
    addDownwardMargin = addDownwardMargin,
    addUpwardMargin = addUpwardMargin,
    addExportAndImport = addExportAndImport,
    addLoadFactorLink = addLoadFactorLink,
    externalDependency = externalDependency,
    loadFactor = loadFactor,
    modulation = modulation,
    netLoadRamp = netLoadRamp,
    surplus = surplus,
    surplusClusters = surplusClusters)
  
  columnsToAdd <- .getNewColumnsName(allStraitments)
  writeAreas <- ifelse(is.null(columnsToAdd$areas) & length(evalAreas) == 0, FALSE, TRUE)
  writeLinks <- ifelse(is.null(columnsToAdd$links) & length(evalLinks) == 0, FALSE, TRUE)
  writeClusters <- ifelse(is.null(columnsToAdd$clusters) & length(evalClusters) == 0, FALSE, TRUE)
  writeDistricts <- ifelse(is.null(columnsToAdd$districts) & length(evalDistricts) == 0, FALSE, TRUE)
  
  select <- .getSelectAlias(allStraitments)
  
  columnsToSelects <- unique(unlist(lapply(list(evalAreas,evalLinks,evalClusters,  evalDistricts ), function(Z){
    lapply(Z, function(X){
      strsplit(X, "`")
    })
  })))
  
  ##Load first Mcyear
  
  if(mcY == "mcInd")
  {
    mcYear <- opts$mcYears
  }
  if(mcY == "mcAll")
  {
    mcYear <- "mcAll"
  }
  timeStep <- "hourly"
  
  
  if(mcYear[1] != 'mcAll')
  {
    by = nThreads
    mcYear_L <- vector("list", floor((max(mcYear)-1)/by) + 1 )
    for(i in 1:length(mcYear))
    {
      mcYear_L[[floor((i-1)/by) + 1]] <- c(mcYear_L[[floor((i-1)/by) + 1]], mcYear[i])
    }
    mcYear <- mcYear_L
  }else{
    mcYear <- list(mcYear)
  }
  
  if(length(mcYear[[1]]) > 1){
    cl <- makeCluster(length(mcYear[[1]]))
    Parallel = TRUE
    
    clusterExport(cl, c("opts"))
    clusterEvalQ(cl = cl, {
      require(antaresRead)
      antaresRead:::.setAlliasH5()
      opts <- antaresRead::setSimulationPath(opts$h5path)
    })
    
  }else{
    Parallel = FALSE
  }
  
  outToWrite <- lapply(mcYear, function(X){
    if(X[1] == "mcAll"){
      X <- NULL
    }
    
    if(!Parallel)
    {
      myOut <- .readDataEndAddColumn(opts, select = select, mcYears = X, timeStep = timeStep,
                                     evalAreas = evalAreas, evalLinks = evalLinks,
                                     evalClusters = evalClusters, evalDistricts = evalDistricts,
                                     columnsToSelects = columnsToSelects, allStraitments = allStraitments,
                                     writeAreas = writeAreas,
                                     writeLinks = writeLinks,
                                     writeClusters = writeClusters,
                                     writeDistricts = writeDistricts,
                                     columnsToAdd = columnsToAdd)
    }else{
      clusterExport(cl, c("opts", "select", "X",  "timeStep",
                          "evalAreas", "evalLinks",
                          "evalClusters", "evalDistricts",
                          "columnsToSelects","allStraitments",
                          "writeAreas",
                          "writeLinks",
                          "writeClusters",
                          "writeDistricts",
                          "columnsToAdd"), envir = environment())
      myOut <- parSapply(cl, X, function(Y){
        .readDataEndAddColumn(opts, select = select, mcYears = Y, timeStep = timeStep,
                              evalAreas = evalAreas, evalLinks = evalLinks,
                              evalClusters = evalClusters, evalDistricts = evalDistricts,
                              columnsToSelects = columnsToSelects, allStraitments = allStraitments,
                              writeAreas = writeAreas,
                              writeLinks = writeLinks,
                              writeClusters = writeClusters,
                              writeDistricts = writeDistricts,
                              columnsToAdd = columnsToAdd)
      }, simplify = FALSE)
      
      
      namS <- names(myOut[[1]])
      myOut <- sapply(1:length(myOut[[1]]), function(V){
        rbindlist(sapply(1:length(myOut), function(W){
          myOut[[W]][[V]]
        }, simplify = FALSE))
      }, simplify = FALSE)
      names(myOut) <- namS
      
    }
    
    outList <- names(myOut)
    outToWrite <- sapply(outList, function(HH){
      as.matrix(myOut[[HH]])
    }, simplify = FALSE)
    
    
    if(is.null(X)){
      writeStruct <- TRUE
    }else{
      writeStruct <- unique(X == mcYear[[1]])
    }
    
    
    writeAreas <- "areas" %in% names(outToWrite)
    writeLinks <- "links" %in% names(outToWrite)
    writeClusters <- "clusters" %in% names(outToWrite)
    writeDistricts <- "districts" %in% names(outToWrite)
    
    
    .writeAllTables(timeStep = timeStep,
                    mcY = mcY,
                    path = opts$h5path,
                    outToWrite = outToWrite ,
                    areas = writeAreas,
                    links = writeLinks,
                    clusters = writeClusters,
                    districts = writeDistricts,
                    mcYear = X, writeStruct = writeStruct)
    
    
    
  })
  if(Parallel) stopCluster(cl)
  
  
  ##Add control on straitments to define all this objects
  
  ##IfverWiteAreas
  
}

.writeAllTables <- function(timeStep, mcY, path, outToWrite,
                            areas, links, clusters, districts, 
                            mcYear = NULL, writeStruct = FALSE){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  fid <- rhdf5::H5Fopen(path)
  sapply(c("areas", "links", "clusters", "districts"), function(X){
    if(get(X)){
      fid <- rhdf5::H5Fopen(path)
      Y <- eval(X)
      GP <- paste0(timeStep, "/", Y, "/", mcY)
      .writeNewColumns(fid = fid,
                       newdata = outToWrite[[Y]],
                       GP = GP, mcYear = mcYear,
                       writeStruct = writeStruct)
    }
    
  })
  
}


.getDim <- function(fid, GP, type = "size")
{
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  did <- rhdf5::H5Dopen(fid, GP)
  rhdf5::H5Dget_space(did)
  res <- rhdf5::H5Dget_space(did)
  rhdf5::H5Dclose(did)
  dim <- rhdf5::H5Sget_simple_extent_dims(res)[[type]]
  dim
}

.getIndexToWrite <- function(dim, nbVarToWrite, mcYear = NULL){
  d4 <- if(is.null(mcYear)){1:dim[4]}else{mcYear}
  list(1:dim[1], (dim[2] + 1) : (dim[2] + nbVarToWrite), 1:dim[3], d4)
}


.readDataEndAddColumn <- function(opts, select, mcYears, timeStep, 
                                  evalAreas, evalLinks,
                                  evalClusters, evalDistricts, columnsToSelects, 
                                  allStraitments,
                                  writeAreas,
                                  writeLinks,
                                  writeClusters,
                                  writeDistricts, columnsToAdd){
  
  if(writeAreas){
    ar <- "all"
  }else{
    ar <- NULL
  }
  if(writeLinks){
    ln <- "all"
  }else{
    ln <- NULL
  }
  if(writeClusters){
    clu <- "all"
  }else{
    clu <- NULL
  }
  if(writeDistricts){
    dr <- "all"
  }else{
    dr <- NULL
  }
  res <- readAntares(areas = ar, 
                     links = ln,
                     clusters = clu,
                     districts = dr,
                     opts = opts, select = c(select,columnsToSelects),
                     mcYears = mcYears, timeStep = timeStep)
  res <- as.antaresDataList(res)
  nrowRes <- lapply(res, nrow)
  
  
  # for(i in 1:length(res)){
  #   res[[i]] <- res[[i]][, .SD, .SDcols = names(res[[i]])[!names(res[[i]])%in%select]]
  # }
  res <- .calcNewColumns(res, allStraitments, timeStep = timeStep)
  
  if(writeAreas && "areas" %in% names(res)){
    if(length(evalAreas) > 0)
    {
      res$areas[, names(evalAreas) := lapply(evalAreas, function(X){eval(parse(text = X))})]
    }
    
    cAdd <- c(columnsToAdd$areas, names(evalAreas))
    
    res$areas <- res$areas[, .SD, .SDcols = cAdd[cAdd%in%names(res$areas)]]
  }else{
    res$areas <- NULL
  }
  if(writeLinks && "links" %in% names(res)){
    if(length(evalLinks) > 0)
    {
      res$links[, names(evalLinks) := lapply(evalLinks, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$links, names(evalLinks))
    res$links <- res$links[, .SD, .SDcols = cAdd[cAdd%in%names(res$links)]]
  }else{
    res$links <- NULL
  }
  if(writeClusters && "clusters" %in% names(res)){
    if(length(evalClusters) > 0)
    {
      res$clusters[, names(evalClusters) := lapply(evalClusters, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$clusters,names(evalClusters))
    res$clusters <- res$clusters[, .SD, .SDcols =  cAdd[cAdd%in%names(res$clusters)]]
  }else{
    res$clusters <- NULL
  }
  if(writeDistricts && "districts" %in% names(res)){
    if(length(evalDistricts) > 0)
    {
      res$districts[, names(evalDistricts) := lapply(evalDistricts, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$links, names(evalAreas))
    res$districts <- res$districts[, .SD, .SDcols =  cAdd[cAdd%in%names(res$districts)]]
  }else{
    res$districts <- NULL
  }
  
  
  ###Controle data write
  lapply(res, function(X){
    classColumns <- unlist(lapply(X, class))
    wolumnstoTransform <- which(classColumns == "logical")
    if(length(wolumnstoTransform) > 0){
      X[,names(wolumnstoTransform) := lapply(X = .SD, as.numeric), .SDcols = wolumnstoTransform]
      if(mcYears[1] == opts$mcYears[1])
      {
        cat(paste0("Some boolean column(s) found, they will be transform to numeric (TRUE : 1, FALSE : 0)"))
      }
    }
  })
  
  lapply(res, function(GG){
    if(!all(unlist(lapply(GG, class)) %in% c("numeric", "integer"))){
      concerCol <- names(unlist(lapply(GG, class)))[
        !unlist(lapply(GG, class)) %in% c("numeric", "integer")]
      stop("Somes columns (", paste0(concerCol, collapse = ";") ,") are not numeric, integer or logical they can't be write in h5")
    }
  })
  
  nrwNrowRes <- lapply(res, nrow)
  for(i in names(nrwNrowRes)){
    if(nrowRes[[i]] != nrwNrowRes[[i]]){
      stop("New file have a diffrent number of row than request file, columns can't be add to h5 file")
    }
  }
  
  res
}


.writeNewColumns <- function(fid, newdata, GP, mcYear = NULL, writeStruct = FALSE)
{
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  
  nbVarToWrite <- ncol(newdata)
  namesVariable <- colnames(newdata)
  datatype <- paste0(GP, "/data")
  if(writeStruct)
  {
    oldStruct <-  paste0(GP, "/structure/reCalcVar")
    did <- rhdf5::H5Dopen(fid, oldStruct)
    structVarAdd <- rhdf5::H5Dread(did)
    rhdf5::H5Dclose(did)
    if(sum(namesVariable %in% structVarAdd) > 0 )
    {
      cat("Somes columns already exists in h5 file, they will be overwrite")
      namesVariable <- namesVariable[!namesVariable %in% structVarAdd]
      
    }
    if(length(namesVariable) > 0){
      structVarAdd[which(structVarAdd == "")][1:length(namesVariable)] <- namesVariable
      
      #h5write(structVarAdd, path, oldStruct)
      rhdf5::h5writeDataset(obj = structVarAdd,  fid, oldStruct)
      

      # if(grepl("areas", GP))
      # {
      #   attributes <- .loadAttributes(fid, "hourly")
      #   attributes$opts$variables$areas <- unique(c(attributes$opts$variables$areas, namesVariable))
      #   .writeAttributes(path = NULL, timeStep =  "hourly", fid = fid, attributes = attributes)
      #   
      # }
      # 
      # if(grepl("links", GP))
      # {
      #   attributes <- .loadAttributes(fid, "hourly")
      #   attributes$opts$variables$links <- unique(c(attributes$opts$variables$links, namesVariable))
      #   .writeAttributes(path = NULL, timeStep =  "hourly", fid = fid, attributes = attributes)
      #   
      # }
      
    }
  }
  
  oldStruct <-  paste0(GP, "/structure/reCalcVar")
  did <- rhdf5::H5Dopen(fid, oldStruct)
  allVarAdd <- rhdf5::H5Dread(did )
  
  
  oldStruct <-  paste0(GP, "/structure/variable")
  did <- rhdf5::H5Dopen(fid, oldStruct)
  allnorm <- rhdf5::H5Dread(did )
  allVarAdd <- c(allnorm, allVarAdd)
  indexVar <- sapply(colnames(newdata), function(X){
    which(allVarAdd == X)
  })
  
  
  actualDim <- .getDim(fid, datatype)
  indexToWrite <- .getIndexToWrite(actualDim, nbVarToWrite, mcYear)
  dimtowrite <- unlist(lapply(indexToWrite, length))
  indexToWrite[[2]] <- indexVar
  
  arrayToWrite <- array(newdata, dimtowrite[c(1,3,4,2)])
  # dim(arrayToWrite)
  arrayToWrite <- aperm(arrayToWrite, c(1,4,2,3))
  
  newDim <- actualDim
  newDim[2] <- newDim[2] + dimtowrite[2]
  if(writeStruct && length(namesVariable)>0)
  {
    rhdf5::h5set_extent(fid, datatype, c(newDim))
  }
  rhdf5::h5writeDataset.array(obj = arrayToWrite, fid, datatype, index = indexToWrite)
  rhdf5::H5close()
}


.getNewColumnsName <- function(allStraitments)
{
  areas <- NULL
  links <- NULL
  clusters <- NULL
  districts <- NULL
  for(X in pkgEnvAntareasH5$processDispo$fctname){
    if(get(paste0("allStraitments"))[[X]]){
      areas <- c(areas, pkgEnvAntareasH5$process[[X]]$areas)
      links <- c(links, pkgEnvAntareasH5$process[[X]]$links)
      clusters <- c(clusters, pkgEnvAntareasH5$process[[X]]$clusters)
      districts <- c(districts, pkgEnvAntareasH5$process[[X]]$districts)
      
    }
  }
  list(areas = areas,
       links = links,
       clusters = clusters,
       districts = districts)
}

.getSelectAlias <- function(allStraitments){
  as.character(pkgEnvAntareasH5$processDispo[pkgEnvAntareasH5$processDispo$fctname%in%
                                               names(which(unlist(allStraitments))),]$trtName)
}


.calcNewColumns <- function(res, allStraitments, timeStep){
  
  if(!requireNamespace("antaresProcessing")) stop("Can't addStraitments to .h5 file. Please install the 'antaresProcessing' package.")
  
  oldw <- getOption("warn")
  options(warn = -1)
  if(allStraitments$addDownwardMargin){
    try({
      res <- antaresProcessing::addDownwardMargin(res)
    })
  }
  if(allStraitments$addUpwardMargin){
    try({
      res <- antaresProcessing::addUpwardMargin(res)
    })
  }
  if(allStraitments$addExportAndImport){
    try({
      res$links$loadFactor <- NULL
      res$areas$export <- NULL
      res$areas$import <- NULL
      res <- antaresProcessing::addExportAndImport(res)
    })
  }
  if(allStraitments$addLoadFactorLink){
    try({
      res <- antaresProcessing::addLoadFactorLink(res)
    })
  }
  if(allStraitments$externalDependency){
    try({
      res$areas[,"netLoadRamp" := NULL]
      res$areas[,"netLoad" := NULL]
      res <- antaresProcessing::addNetLoad(res)
    })
    try({
      extDep <- antaresProcessing::externalDependency(res, timeStep =  timeStep)
      
      idC <- getIdCols(extDep)
      res$areas <- merge(res$areas, extDep, by = idC)
    })
  }
  if(allStraitments$loadFactor){
    try({
      loadFactor <- antaresProcessing::loadFactor(res, timeStep =  timeStep)
      idC <- getIdCols(loadFactor)
      res$clusters <- merge(res$clusters, loadFactor, by = idC)
    })
  }
  if(allStraitments$modulation){
    try({
      mod <- antaresProcessing::modulation(res, timeStep =  timeStep)
      
      idC <- getIdCols(mod)
      res$clusters <- merge(res$clusters, mod, by = idC)
    })
  }
  if(allStraitments$netLoadRamp){
    try({
      netLoadRamp <- antaresProcessing::netLoadRamp(res, timeStep = timeStep)
      
      idC <- getIdCols(netLoadRamp)
      res$areas <- merge(res$areas, netLoadRamp, by = idC)
    })
  }
  if(allStraitments$surplus){
    try({
      surplus <- antaresProcessing::surplus(res, timeStep = timeStep)
      
      idC <- getIdCols(surplus)
      res$areas <- merge(res$areas, surplus, by = idC)
    })
  }
  if(allStraitments$surplusClusters){
    try({
      surplusClusters <- antaresProcessing::surplusClusters(res, timeStep =  timeStep)
      idC <- getIdCols(surplusClusters)
      res$clusters <- merge(res$clusters, surplusClusters, by = idC)
    })
  }
  options(warn = oldw)
  res
}

.setAlliasH5 <- function(){
  sapply(names(pkgEnvAntareasH5$process), function(X){
    tpAlias <- pkgEnvAntareasH5$process[[X]]
    X <- paste0("Out_", X)
    sapply(names(tpAlias), function(Y){
      varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
    })
  })
}


# library(antaresProcessing)
# library(data.table)
# devtools::load_all(".")
# path <- "D:/Users/titorobe/Desktop/Antares/antaresHdf5"
# opts <- setSimulationPathH5(path)
# addStraitments(opts,addDownwardMargin = TRUE)
# timeStep = "hourly"
# addDownwardMargin = TRUE
# addUpwardMargin = TRUE
# addExportAndImport = TRUE
# addLoadFactorLink = TRUE
# externalDependency = TRUE
# loadFactor = TRUE
# modulation = TRUE
# netLoadRamp = TRUE
# surplus = TRUE
# surplusClusters = TRUE
# opts <- setSimulationPath("D:/Users/titorobe/Desktop/Antares/antaresHdf5", 1)
# mcY = "mcInd"
