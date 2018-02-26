#' Write antares Study to a .h5 file
#'
#' @param data \code{antaresDataList}
#' @param path \code{character} path of h5 file
#' @param rootGroup \code{character} group will contain all h5 organization
#' @param writeStructure \code{boolean}, write group and subgroup (only for first MCyear)
#' @param mcAll \code{character}, write mcAll
#' @param compress \code{numeric}, compression level
#'
#' 
#' @noRd
#' 
writeAntaresData <- function(data,
                             path,
                             rootGroup = NULL,
                             writeStructure = TRUE,
                             mcAll = FALSE,
                             compress = 0){

  if(!is.null(data$areas)){
    #Write areas
    writeDataType(data = data, path = path,  type = "areas", rootGroup = rootGroup, writeStructure = writeStructure,
                  mcAll = mcAll, compress = compress)
  }

  if(!is.null(data$links)){
    #Write links
    writeDataType(data = data, path = path,  type = "links", rootGroup = rootGroup, writeStructure = writeStructure,
                  mcAll = mcAll, compress = compress)
  }
  if(!is.null(data$districts)){
    #Write districts
    writeDataType(data = data, path = path,  type = "districts", rootGroup = rootGroup, writeStructure = writeStructure,
                  mcAll = mcAll, compress = compress)
  }

  if(!is.null(data$clusters)){
    #Write clusters
    writeDataType(data = data, path = path,  type = "clusters", rootGroup = rootGroup, writeStructure = writeStructure,
                  mcAll = mcAll, compress = compress)
  }

}

#' Write data by type
#'
#' @param data \code{antaresDataList}
#' @param path \code{character} patch of h5 file
#' @param type \code{character} type of data to write, must be areas, links, districts or clusters
#' @param rootGroup \code{character} group will contain all h5 organization
#' @param writeStructure \code{boolean}, write group and subgroup (only for first MCyear)
#' @param mcAll \code{character}, write mcAll
#' @param compress \code{boolean}, compress level
#'
#' @noRd
#' 
writeDataType <- function(data,
                      path,
                      type,
                      rootGroup = NULL,
                      writeStructure = TRUE,
                      mcAll = FALSE,
                      compress = 0){

  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(!mcAll)
  {
    mcYears <- attr(data, "opts")$mcYears
  }else{
    mcYears <- "all"
  }

  data <- data[[type]]
  if(type == "clusters"){
    data$cluster <- paste0(data$area, "/", data$cluster)
    data[,"area" := NULL]
  }


  Group <- paste0(rootGroup, "/", type)
  #Create group by type of data
  if(writeStructure & !mcAll){
    rhdf5::h5createGroup(path, Group)
  }

  #Control if we write mcAll or mcInd
  if(mcAll){
    Group <- paste0(Group, "/", "mcAll")
  }else{
    Group <- paste0(Group, "/", "mcInd")

  }

  #Create group for mc-ind or mc-all
  if(writeStructure){
    rhdf5::h5createGroup(path, Group)
  }
  # print(data)
  #Give structure for data
  dimPreBuild <- names(data)[!names(data)%in%c("mcYear", "V1")]
  dimStructure <- list()
  dimStructure$variable <- names(data$V1[[1]])
  dimStructure <- c(dimStructure, sapply(dimPreBuild, function(X){
    as.character(unique(unlist(data[, .SD, .SDcols = X])))
  }, simplify = FALSE))
  dimStructure$mcYear <- mcYears

  #Give dim length
  nbDim <- length(dimStructure) + 1
  nbTimeId <- nrow(data$V1[[1]])
  dimData <- unlist(c(nbTimeId, lapply(dimStructure, length)))


  #Create array
  groupData <- paste0(Group, "/data")
  structData <- paste0(Group, "/structure")
  if(writeStructure){
    rhdf5::h5createDataset(path, groupData, dims = dimData, chunk = c(dimData[1], 1, 1, 1),
                    level = compress, maxdims = c(dimData[1],
                                                  dimData[2] + 300,
                                                  dimData[3],
                                                  dimData[4]))
    fid <- rhdf5::H5Fopen(path)
    # dimStructure$reCalcVar <- rep("NoVariable", 100)
    rhdf5::h5writeDataset.list(dimStructure, fid, structData, level = compress)
    rhdf5::H5Fclose(fid)
    structNew <- paste0(structData, "/reCalcVar")
    rhdf5::h5createDataset(path, structNew, storage.mode = "character", level = compress , dims = 300,
                    size = 200)

  
  }

  #Convert data to an array
  arrayDatatowrite <- array(dim = dimData[1:(length(dimData)-1)])
  for(i in 1:nrow(data)){
    arrayDatatowrite[, , i] <- as.matrix(data$V1[[i]])
  }

  #Control index for write
  index <- lapply(1:length(dim(arrayDatatowrite)), function(X)NULL)

  if(!mcAll)
  {
    index$LastDim <- which(data$mcYear[1] == mcYears)
  }else{
    index$LastDim  <- 1
  }

  #Write data
  fid <- rhdf5::H5Fopen(path)
  rhdf5::h5writeDataset.array(obj = arrayDatatowrite, fid, groupData, index = index)
  rhdf5::H5Fclose(fid)
  NULL
}
