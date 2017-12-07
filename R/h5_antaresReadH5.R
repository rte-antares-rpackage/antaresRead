#' Read data
#'
#' @param path {character} path of h5file to load
#' @param areas see \link[antaresRead]{readAntares}
#' @param links see \link[antaresRead]{readAntares}
#' @param clusters see \link[antaresRead]{readAntares}
#' @param districts see \link[antaresRead]{readAntares}
#' @param mcYears see \link[antaresRead]{readAntares}
#' @param timeStep see \link[antaresRead]{readAntares}
#' @param select see \link[antaresRead]{readAntares}
#' @param showProgress see \link[antaresRead]{readAntares}
#' @param simplify see \link[antaresRead]{readAntares}
#' @param misc see \link[antaresRead]{readAntares}
#' @param thermalAvailabilities see \link[antaresRead]{readAntares}
#' @param hydroStorage see \link[antaresRead]{readAntares}
#' @param hydroStorageMaxPower see \link[antaresRead]{readAntares}
#' @param reserve see \link[antaresRead]{readAntares}
#' @param linkCapacity see \link[antaresRead]{readAntares}
#' @param mustRun see \link[antaresRead]{readAntares}
#' @param thermalModulation see \link[antaresRead]{readAntares}
#' @param perf \code{boolean}, eval performance during developpement time, to remove
#'
#' @import parallel plyr
#' 
#' @noRd
#' @export
# Need to be export for antaresViz
.h5ReadAntares <- function(path, areas = NULL, links = NULL, clusters = NULL,
                           districts = NULL, mcYears = NULL,
                           misc = FALSE, thermalAvailabilities = FALSE,
                           hydroStorage = FALSE, hydroStorageMaxPower = FALSE, reserve = FALSE,
                           linkCapacity = FALSE, mustRun = FALSE, thermalModulation = FALSE,
                           timeStep = "hourly", select = NULL, showProgress = TRUE,
                           simplify = TRUE, perf = FALSE){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(!is.list(select))
  {
    if(is.null(select))
    {
      if(!is.null(areas))
      {
        if(areas[1] == "all"){
          select <- c("all", select)
        }}
      
      if(!is.null(links))
      {
        if(links[1] == "all"){
          select <- c("all", select)
        }}
      
      if(!is.null(clusters))
      {
        if(clusters[1] == "all"){
          select <- c("all", select)
        }}
      if(!is.null(districts))
      {
        if(districts[1] == "all"){
          select <- c("all", select)
        }}
    }else{
      if("allAreas" %in% select){
        select <- c(select, pkgEnvAntareasH5$varAreas)
      }
      
      if("allLinks" %in% select){
        select <- c(select, pkgEnvAntareasH5$varLinks)
      }
      
      if("allDistricts" %in% select){
        select <- c(select, pkgEnvAntareasH5$varDistricts)
      }
    }
  }
  
  
  if(!file.exists(path)){
    stop(paste0("File ", path, " not exist."))
  }
  
  if(perf){
    Beg <- Sys.time()
  }
  
  ctrlselectlist <- FALSE
  if(!is.list(select)){
    ctrlselectlist <- TRUE
  }
  
  
  if(ctrlselectlist){
    if(misc){
      select <- c(select, "misc")
    }
    if(thermalAvailabilities){
      select <- c(select, "thermalAvailabilities")
    }
    if(hydroStorage){
      select <- c(select, "hydroStorage")
    }
    if(hydroStorageMaxPower){
      select <- c(select, "hydroStorageMaxPower")
    }
    if(reserve){
      select <- c(select, "reserve")
    }
    if(linkCapacity){
      select <- c(select, "linkCapacity")
    }
    if(mustRun){
      select <- c(select, "mustRun")
    }
    if(thermalModulation){
      select <- c(select, "thermalModulation")
    }
  }
  
  
  if(is.null(select)){
    select <- "all"
  }
  reqInfos <- .giveInfoRequest(select = select,
                               areas = areas,
                               links = links,
                               clusters = clusters,
                               districts = districts,
                               mcYears = mcYears)
  select <- reqInfos$select
  if(ctrlselectlist){
    select$clusters <- c(pkgEnvAntareasH5$varClusters, select$areas)
  }
  
  
  unselect <- reqInfos$unselect
  
  
  
  allCompute <- pkgEnv$allCompute
  computeAdd <- unlist(select)[unlist(select) %in% allCompute]
  computeAdd <- unique(computeAdd)
  if(length(computeAdd) > 0){
    for(i in computeAdd)
    {
      assign(i, TRUE)
    }
  }
  
  for(i in allCompute){
    if(get(i)){
      select <- .addColumns(select, i)
    }
  }
  
  select <- sapply(names(select), function(X){
    as.vector(unlist(sapply(select[[X]], function(Y){
      if(is.null(pkgEnvAntareasH5$varAliasCreated[[Y]][[X]])){
        Y
      }else{
        pkgEnvAntareasH5$varAliasCreated[[Y]][[X]]
      }
    })))
  }, simplify = FALSE)
  
  ctrl <- FALSE
  if(!is.null(select$areas))
  {
    if(select$areas[1] == "all"){
      ctrl <- TRUE
    }
  }
  if(is.null(select$areas) | ctrl)
  {
    select$areas <- c(pkgEnvAntareasH5$varAreas,select$areas)
  }
  
  ctrl <- FALSE
  if(!is.null(select$links))
  {
    if(select$links[1] == "all"){
      ctrl <- TRUE
    }
  }
  
  if(is.null(select$links)| ctrl)
  {
    select$links <- c(pkgEnvAntareasH5$varLinks, select$links)
  }
  
  ctrl <- FALSE
  if(!is.null(select$districts))
  {
    if(select$districts[1] == "all"){
      ctrl <- TRUE
    }
  }
  
  if(is.null(select$districts) | ctrl)
  {
    select$districts <- c(pkgEnvAntareasH5$varDistricts,  select$districts )
  }
  ctrl <- FALSE
  if(!is.null(select$clusters))
  {
    if(select$clusters[1] == "all"){
      ctrl <- TRUE
    }
  }
  
  if(is.null(select$clusters) | ctrl)
  {
    select$clusters <- c(pkgEnvAntareasH5$varClusters, select$clusters )
  }
  
  
  for(i in names(select)){
    if(length(which(! select[[i]] %in% unselect[[i]])) > 0)
    {
      select[[i]] <- select[[i]][which(! select[[i]] %in% unselect[[i]])]
    }
  }
  
  for(i in 1:length(select)){
    if(length(select[[i]]) > 1){
      if(length(which(select[[i]] == "all")) > 0){
        select[[i]] <- select[[i]][-c(which(select[[i]] == "all"))]
      }
    }
  }
  
  ##End give select
  
  areas <- reqInfos$areas
  links <- reqInfos$links
  clusters <- reqInfos$clusters
  districts <- reqInfos$districts
  mcYears <- reqInfos$mcYears
  synthesis <- reqInfos$synthesis
  
  synthesis <- is.null(mcYears)
  
  GP <- timeStep
  
  ##Open connection to h5 file
  fid <- rhdf5::H5Fopen(path)
  
  #Load attibutes
  attrib <- .loadAttributes(fid, timeStep)
  
  if(is.null(mcYears)){
    mcType <- "mcAll"
    mcYears <- "mcAll"
  }else{
    mcType <- "mcInd"
  }
  
  ##Load areas
  listOut <- list()
  areas <- .loadAreas(areas = areas,
                      fid = fid,
                      select = select$areas,
                      mcYears = mcYears,
                      GP = GP,
                      mcType = mcType,
                      synthesis = synthesis,
                      simplify = simplify,
                      attrib = attrib)
  # if("virtualNodes" %in% names(attrib)){
  #   attr(areas, "virtualNodes") <- attrib$virtualNodes
  # }
  # 
  if(!is.null(areas)){
    listOut$areas <- areas
    rm(areas)
  }
  
  links <- .loadLinks(links = links,
                      fid = fid,
                      select = select$links,
                      mcYears = mcYears,
                      GP = GP,
                      mcType = mcType,
                      synthesis = synthesis,
                      simplify = simplify,
                      attrib = attrib)
  

  if(!is.null(links)){
    listOut$links <- links
    rm(links)
  }
  
  districts <- .loadDistricts(districts = districts,
                              fid = fid,
                              select = select$districts,
                              mcYears = mcYears,
                              GP = GP,
                              mcType = mcType,
                              synthesis = synthesis,
                              simplify = simplify,
                              attrib = attrib)
  
  if(!is.null(districts)){
    listOut$districts <- districts
    rm(districts)
  }
  clusters <- .loadClusters(clusters = clusters,
                            fid = fid,
                            select = select$clusters,
                            mcYears = mcYears,
                            GP = GP,
                            mcType = mcType,
                            synthesis = synthesis,
                            simplify = simplify,
                            attrib = attrib)
  
  if(!is.null(clusters)){
    listOut$clusters <- clusters
    rm(clusters)
  }
  
  if(length(listOut) == 1){
    
    if(perf){
      TotalTime <-Sys.time() - Beg
      cat(paste0("Time for loading : ", round(TotalTime, 3), "\n"))
      objectS <- utils::object.size(listOut)/1024^2
      cat(paste0("Size of object loaded : ", round(objectS, 1), "Mo\n"))
      cat(paste0("Mo/s loaded : ",round(as.numeric(objectS)/ as.numeric(TotalTime),1), "\n"))
      dtaloded <- sum(unlist(lapply(listOut, function(X)prod(dim(X)))))
      cat(paste0("Data loaded/s : ", round(dtaloded/ as.numeric(TotalTime) / 1000000, 2), " Millions", "\n"))
    }
    
    listOut[[1]]
  }else{
    listOut <- .addClassAndAttributes(listOut, synthesis, timeStep,
                                      attrib$opts, simplify)
    if("virtualNodes" %in% names(attrib)){
      attr(listOut, "virtualNodes") <- attrib$virtualNodes
    }
    
    if(perf){
      TotalTime <-Sys.time() - Beg
      cat(paste0("Time for loading : ", round(TotalTime, 3), "\n"))
      objectS <- utils::object.size(listOut)/1024^2
      cat(paste0("Size of object loaded : ", round(objectS, 1), "Mo\n"))
      cat(paste0("Mo/s loaded : ",round(as.numeric(objectS)/ as.numeric(TotalTime),1), "\n"))
      dtaloded <- sum(unlist(lapply(listOut, function(X)prod(dim(X)))))
      cat(paste0("Data loaded/s : ", round(dtaloded/ as.numeric(TotalTime) / 1000000, 2), " Millions", "\n"))
    }
    
    listOut
  }
}




#' Transform array to data.table
#'
#' @param array \code{array}, array to transform
#'
#' @return data.table
#'
#' @noRd
.arrayToDataTable <- function(array)
{
  dim <- 2
  ecraseDim <- dim(array)[dim]
  dimS <- 1:length(dim(array))
  dimNot <- dimS[-dim]
  prodDim <- prod(dim(array)[dimNot])
  arrayL <- list()
  for(i in 1:dim(array)[2]){
    arrayL[[i]] <- unlist(array[,i,,])
  }
  setattr(arrayL, "names", paste("V", 1:ecraseDim, sep = ""))
  setattr(arrayL, "row.names", .set_row_names(prodDim))
  setattr(arrayL, "class", c("data.table", "data.frame"))
  alloc.col(arrayL)
}

#' Load antares simulation data
#'
#' @param fid \code{H5IdComponent} id of h5 file open which \link{rhdf5::H5Fopen}
#' @param index \code{list} index of data to load
#' @param GP \code{character} name of group to load
#'
#' @noRd
.optimH5Read <- function(fid, index = NULL, GP){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  did <- rhdf5::H5Dopen(fid,  GP)
  if(is.null(index)){
    return(rhdf5::H5Dread(did))
  }else{
    
    h5spaceFile <- rhdf5::H5Dget_space(did)
    maxSize <- rev(rhdf5::H5Sget_simple_extent_dims(h5spaceFile)$size)
    
    len <- length(maxSize)
    K <-  sapply(len:1, function(X){
      if(is.null(index[[len-X + 1]])){
        seq_len(maxSize[X])
      }else{index[[len-X + 1]]}}
    )
    size <- unlist(lapply(K,length))
    h5spaceMem = rhdf5::H5Screate_simple(size)
    W <- rhdf5::H5Screate_simple(rhdf5::H5Sselect_index(h5spaceFile, as.list(K)))@ID
    
    rhdf5::H5Dread(did,  h5spaceFile = h5spaceFile,
                   h5spaceMem = h5spaceMem)
  }
  
}

#' Give request stucture
#'
#' @param type \code{character} type of request, must be area, link, cluster or district
#' @param selectedRow \code{character} selectoin on raw (country, link, cluster ....)
#' @param selectedCol \code{character} columns to select
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param mcYears \code{numeric or character} mcYears to laod
#'
#' @noRd
.makeStructure <- function(type = "area", selectedRow,
                           selectedCol, fid, GP, mcType, mcYears){
  if(is.null(selectedCol)){
    selectedCol <- "all"
  }
  
  typeS <- paste0(type, "s")
  struct <- .getstructure(fid, paste0(GP, "/", typeS, "/", mcType, "/structure"))
  
  compname <- NULL
  if(type == "cluster"){
    splitClust <- strsplit(struct[[type]], "/")
    clusterClean <- unlist(lapply(splitClust, function(X){X[1]}))
    struct[[type]] <- clusterClean
    compname <- unlist(lapply(splitClust, function(X){X[2]}))
    
  }
  
  if(selectedRow[1] == "all"){
    indexType  <- NULL
    Name <- struct[[type]]
  }else{
    indexType <- which(struct[[type]] %in% selectedRow)
    Name <- struct[[type]][indexType]
    if(type == "cluster"){
      compname <- compname[indexType]
    }
  }
  if(selectedCol[1] == "all"){
    indexVar <- NULL
    varKeep <- struct$variable
  }else{
    indexVar <- which(struct$variable %in% selectedCol)
    indexVar <- unique(c(1, indexVar))
    varKeep <- struct$variable[indexVar]
  }
  if(mcYears[1] == "all"){
    indexMC <- NULL
    mcyLoad <- struct$mcYear
  }else{
    if(mcYears[1] == "mcAll"){
      indexMC <- NULL
      mcyLoad <- struct$mcYear
    }else{
      indexMC <- which(struct$mcYear %in% mcYears)
      mcyLoad <- struct$mcYear[indexMC]
    }
  }
  return(list(Name = Name, varKeep = varKeep, index = list(NULL, indexVar, indexType, indexMC),
              mcyLoad = mcyLoad, compname = compname))
}

#' Load areas
#'
#' @param areas \code{character}, area(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to load
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#' @param attrib \code{list}
#' 
#' @noRd
.loadAreas <- function(areas,
                       fid,
                       select,
                       mcYears,
                       GP,
                       mcType,
                       synthesis,
                       simplify,
                       attrib){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(!is.null(areas)){
    
    if(rhdf5::H5Lexists(fid, paste0(GP, "/areas/", mcType, "/structure")))
    {
      
      struct  <- .makeStructure(type = "area",
                                selectedRow = areas,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)
      
      
      if(all(unlist(lapply(struct$index, is.null)))){
        areas <-  .optimH5Read(fid = fid,
                               GP = paste0(GP, "/areas/", mcType, "/data"))
      }else{
        areas <- .optimH5Read(fid = fid,
                              index = struct$index,
                              GP = paste0(GP, "/areas/", mcType, "/data"))
        
      }
      
      
      #Format array
      areas <- .formatArray(data = areas, struct = struct, nameColumns = "area", mcType = mcType)
      
      #Add time
      tim <- getAllDateInfoFromDate(fid, GP)
      areas[,c(names(tim)):=tim]
      
      .addClassAndAttributes(areas,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "areas")
      areas
    }else{
      message("No data corresponding to your areas query.")
      return(NULL)
    }
  }else{NULL}}

#' Load links
#'
#' @param links \code{character}, link(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to load
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#' @param attrib \code{list}
#' 
#' @noRd
.loadLinks <- function(links,
                       fid,
                       select,
                       mcYears,
                       GP,
                       mcType,
                       synthesis,
                       simplify,
                       attrib){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  ##Load links
  if(!is.null(links)){
    
    if(rhdf5::H5Lexists(fid, paste0(GP, "/links/", mcType, "/structure")))
    {
      
      
      struct  <- .makeStructure(type = "link",
                                selectedRow = links,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)
      
      
      if(all(unlist(lapply(struct$index, is.null)))){
        links <-  .optimH5Read(fid = fid,
                               GP = paste0(GP, "/links/", mcType, "/data"))
      }else{
        links <- .optimH5Read(fid = fid,
                              index = struct$index,
                              GP = paste0(GP, "/links/", mcType, "/data"))
        
      }
      
      #Format array
      links <- .formatArray(data = links, struct = struct, nameColumns = "link", mcType = mcType)
      
      #Add time
      tim <- getAllDateInfoFromDate(fid, GP)
      links[,c(names(tim)):=tim]
      
      .addClassAndAttributes(links,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "links")
      links
    }else{
      message("No data corresponding to your links query.")
      return(NULL)
    }
  }else{NULL}
}



#' Load districts
#'
#' @param districts \code{character}, district(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to load
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#' @param attrib \code{list}
#' 
#' @noRd
.loadDistricts <- function(districts,
                           fid,
                           select,
                           mcYears,
                           GP,
                           mcType,
                           synthesis,
                           simplify,
                           attrib){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(!is.null(districts)){
    
    if(rhdf5::H5Lexists(fid, paste0(GP, "/districts/", mcType, "/structure")))
    {
      
      struct  <- .makeStructure(type = "district",
                                selectedRow = districts,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)
      
      
      if(all(unlist(lapply(struct$index, is.null)))){
        districts <-  .optimH5Read(fid = fid,
                                   GP = paste0(GP, "/districts/", mcType, "/data"))
      }else{
        districts <- .optimH5Read(fid = fid,
                                  index = struct$index,
                                  GP = paste0(GP, "/districts/", mcType, "/data"))
        
      }
      
      
      districts <- .formatArray(data = districts, struct = struct, nameColumns = "district", mcType = mcType)
      
      tim <- getAllDateInfoFromDate(fid, GP)
      
      #Add time
      districts[,c(names(tim)):=tim]
      
      .addClassAndAttributes(districts,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "districts")
      districts
    }else{
      message("No data corresponding to your districts query.")
      return(NULL)
    }}else{NULL}
}




#' Load clusters
#'
#' @param clusters \code{character}, cluster(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link{rhdf5::H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to load
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#' @param attrib \code{list}
#' 
#' @noRd
.loadClusters <- function(clusters,
                          fid,
                          select,
                          mcYears,
                          GP,
                          mcType,
                          synthesis,
                          simplify,
                          attrib){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(!is.null(clusters)){
    
    if(rhdf5::H5Lexists(fid, paste0(GP, "/clusters/", mcType, "/structure")))
    {
      
      
      struct  <- .makeStructure(type = "cluster",
                                selectedRow = clusters,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)
      
      if(all(unlist(lapply(struct$index, is.null)))){
        clusters <-  .optimH5Read(fid = fid,
                                  GP = paste0(GP, "/clusters/", mcType, "/data"))
      }else{
        clusters <- .optimH5Read(fid = fid,
                                 index = struct$index,
                                 GP = paste0(GP, "/clusters/", mcType, "/data"))
        
      }
      
      
      dimclusters <- dim(clusters)
      clusters <- .formatArray(data = clusters, struct = struct, nameColumns = "area", mcType = mcType)
      
      compname <- as.factor(struct$compname)
      clusters[, "cluster" := rep(rep(compname, each = dimclusters[1]), dimclusters[4])]
      tim <- getAllDateInfoFromDate(fid, GP)
      
      #Add time
      clusters[,c(names(tim)):=tim]
      
      .addClassAndAttributes(clusters,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "clusters")
      clusters
    }else{
      message("No data corresponding to your clusters query.")
      return(NULL)
    }}else{NULL}
}

#' Add structure information to data
#'
#' @param data \code{data.table} data load
#' @param struct \code{list}
#' @param nameColumns \code{character} column names
#' @param mcType \code{character}, must be mcInd, and mcAll
#'
#' @noRd
.formatArray <- function(data, struct, nameColumns, mcType){
  dimData <- dim(data)
  data <- .arrayToDataTable(data)
  nameS <- struct$varKeep
  names(data) <- nameS
  dataName <- as.factor(struct$Name)
  data[, c(nameColumns[1]):= rep(rep(dataName, each = dimData[1]), dimData[4])]
  if(mcType == "mcInd")
  {
    data[, "mcYear" := rep(struct$mcyLoad, each = dimData[1] * dimData[3])]
  }
  
  integerVariableS <- integerVariable[integerVariable%in%names(data)]
  if("timeId" %in% names(data)){
    integerVariableS <- c("timeId", integerVariableS)
  }
  
  # if(length(integerVariableS)){
  #   ordervar <- names(data)[ match(integerVariableS, names(data))]
  #   data[,c(ordervar) := lapply(.SD, as.integer), .SDcols = ordervar]
  # }
  data
}

#' @param select select column(s)
#' @param var var to add
#'
#' @noRd
.addColumns <- function(select, var){
  if(is.null(select)){
    return(var)
  }
  if(is.list(select)){
    return(lapply(select, function(X){c(X, var)}))
  }
  c(var, select)
}


.loadAttributes <- function(fid, timeStep){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/attrib")))
  {
    
    did <- rhdf5::H5Dopen(fid, paste0(timeStep, "/attrib"))
    attrib <- unserialize(charToRaw(rhdf5::H5Dread(did)))
    rhdf5::H5Dclose(did)
    
    if(!is.null(attrib$opts$linksDef)){
      attrib$opts$linksDef <- data.table(attrib$opts$linksDef)
    }
    if(!is.null(attrib$opts$districtsDef)){
      attrib$opts$districtsDef <- data.table(attrib$opts$districtsDef)
    }
  }else{
    attrib <- NULL
  }
  attrib
}


.getstructure <- function(fid, strgp){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  gid <- rhdf5::H5Gopen(fid,  strgp)
  data <- rhdf5::h5dump(gid)
  rhdf5::H5Gclose(gid)
  if(length(which(data$reCalcVar!="")) > 0)
  {
    data$reCalcVar <- data$reCalcVar[which(data$reCalcVar!="")]
    data$variable <- c(data$variable, data$reCalcVar)
    data$reCalcVar <- NULL
  }
  data
}


#' 
#' #' Use to transform inputs arguments to be passable to reading function
#' #'
#' #'
#' #'
#' #' @param select Character vector containing the name of the columns to import. See \link{readAntares} for further information.
#' #' @param areas Vector containing the names of the areas to import. See \link{readAntares} for further information.
#' #' @param links Vector containing the names of the links to import. See \link{readAntares} for further information.
#' #' @param clusters Vector containing the names of the clusters to import. See \link{readAntares} for further information.
#' #' @param districts Vector containing the names of the districts to import. See \link{readAntares} for further information.
#' #' @param mcYears Index of the Monte-Carlo years to import. See \link{readAntares} for further information.
#' #'
#' #' @return \code{list}
#' #' \itemize{
#' #' \item select
#' #' \item areas
#' #' \item links
#' #' \item clusters
#' #' \item districts
#' #' \item mcYears
#' #' \item synthesis
#' #' \item computeAdd
#' #' }
#' #'
#' #' @noRd
#' .giveInfoRequest <- function(select,
#'                              areas,
#'                              links,
#'                              clusters,
#'                              districts,
#'                              mcYears){
#' 
#'   if (!is.list(select)) select <- list(areas = select, links = select, districts = select)
#'   ##Get unselect columns (by - operator)
#'   unselect <- lapply(select, function(X){
#'     minusColumns <- grep("^-", X)
#'     if(length(minusColumns)>0)
#'     {
#'       uns <- X[minusColumns]
#'       gsub("^-", "", uns)
#'     }else{
#'       NULL
#'     }
#'   })
#' 
#'   ##Remove unselect columns
#'   select <- lapply(select, function(X){
#'     minusColumns <- grep("^-", X)
#'     if(length(minusColumns) > 0){
#'       X[-c(minusColumns)]
#'     }else{
#'       X
#'     }
#'   })
#' 
#' 
#'   # Aliases for groups of variables
#'   select <- llply(select, function(x) {
#'     for (alias in names(pkgEnv$varAliases)) {
#'       if (tolower(alias) %in% tolower(x)) x <- append(x, pkgEnv$varAliases[[alias]]$select)
#'     }
#'     x
#'   })
#' 
#'   allCompute <- pkgEnv$allCompute
#'   computeAdd <- allCompute[allCompute%in%unlist(select)]
#' 
#'   if ("areas" %in% unlist(select) & is.null(areas)) areas <- "all"
#'   if ("links" %in% unlist(select) & is.null(links)) {
#'     if (!is.null(areas)) links <- getLinks(getAreas(areas, regexpSelect = FALSE))
#'     else links <- "all"
#'   }
#'   if ("clusters" %in% unlist(select) & is.null(clusters)) {
#'     if (!is.null(areas)) clusters <- areas
#'     else clusters <- "all"
#'   }
#'   if ("mcYears" %in% unlist(select) & is.null(mcYears)) mcYears <- "all"
#' 
#'   # If all arguments are NULL, import all areas
#'   if (is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)) {
#'     areas <- "all"
#'   }
#' 
#'   # Check arguments validity. The function .checkArgs is defined below
#'   synthesis <- is.null(mcYears)
#' 
#'   return(list(select = select,
#'               areas = areas,
#'               links = links,
#'               clusters = clusters,
#'               districts = districts,
#'               mcYears = mcYears,
#'               synthesis = synthesis,
#'               computeAdd = computeAdd,
#'               unselect = unselect))
#' }

