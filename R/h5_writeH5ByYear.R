#' Convert antares output to h5 file
#'
#' @param path \code{character} folder where h5 file will be write (default getwd())
#' @param timeSteps \code{character} timeSteps
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param writeMcAll \code{boolean} write mc-all
#' @param compress \code{numeric} compress level
#' @param misc \code{boolean} see \link[antaresRead]{readAntares}
#' @param thermalAvailabilities \code{boolean} see \link[antaresRead]{readAntares}
#' @param hydroStorage \code{boolean} see \link[antaresRead]{readAntares}
#' @param hydroStorageMaxPower \code{boolean} see \link[antaresRead]{readAntares}
#' @param reserve \code{boolean} see \link[antaresRead]{readAntares}
#' @param linkCapacity \code{boolean} see \link[antaresRead]{readAntares}
#' @param mustRun \code{boolean} see \link[antaresRead]{readAntares}
#' @param thermalModulation \code{boolean} see \link[antaresRead]{readAntares}
#' @param writeAllSimulations \code{boolean}, write all simulations of your antares study.
#' @param nbCores \code{numeric}, number of cores to use, only used if writeAllSimulations is TRUE
#' @param removeVirtualAreas \code{boolean}, remove virtual areas, see \link[antaresRead]{removeVirtualAreas}
#' @param storageFlexibility \code{character}, see \link[antaresRead]{removeVirtualAreas}
#' @param production \code{character}, see \link[antaresRead]{removeVirtualAreas}
#' @param reassignCosts \code{boolean}, see \link[antaresRead]{removeVirtualAreas}
#' @param newCols \code{boolean}, see \link[antaresRead]{removeVirtualAreas}
#' @param overwrite \code{boolean}, overwrite old file
#' 
#' @examples
#' 
#' \dontrun{
#' # Write simulation one by one
#' setSimulationPath("C:/Users/MyUser/Mystudy", 1)
#' writeAntaresH5()
#'
#' # Write all simulations
#' setSimulationPath("C:/Users/MyUser/Mystudy")
#' writeAntaresH5(writeAllSimulations = TRUE)
#'
#' # Choose timestep to write
#' setSimulationPath("C:/Users/MyUser/Mystudy", 1)
#' writeAntaresH5(timeSteps = "hourly")
#'
#' # Write with additionnal information
#' writeAntaresH5(timeSteps = "hourly",
#'    misc = TRUE, thermalAvailabilities = TRUE,
#'    hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
#'    linkCapacity = TRUE, mustRun = TRUE, thermalModulation = TRUE)
#'
#' }
#'
#' @export
writeAntaresH5 <- function(path = getwd(), timeSteps = c("hourly", "daily", "weekly", "monthly", "annual"),
                           opts = simOptions(),
                           writeMcAll = TRUE,
                           compress = 1,
                           misc = FALSE,
                           thermalAvailabilities = FALSE,
                           hydroStorage = FALSE,
                           hydroStorageMaxPower = FALSE,
                           reserve = FALSE,
                           linkCapacity = FALSE,
                           mustRun = FALSE,
                           thermalModulation = FALSE,
                           writeAllSimulations = FALSE,
                           nbCores = 4,
                           removeVirtualAreas = FALSE,
                           storageFlexibility = NULL,
                           production = NULL,
                           reassignCosts = FALSE,
                           newCols = TRUE, 
                           overwrite = FALSE){
  
  if(!dir.exists(path)){
    stop(paste0("Folder ", path, " not found."))
  }
  
  if(!writeAllSimulations){
    simName <- unlist(strsplit(opts$simPath, "/"))
    simName <- simName[length(simName)]
    path <- paste0(path, "/", simName, ".h5")
    
    if(overwrite & file.exists(path)){
      file.remove(path)
    }
    
    if(file.exists(path)){
      stop(paste0("File ", path, " already exist you must use overwrite argument if you want to overwrite"))
    }
    
    
    .writeAntaresH5Fun(path = path,
                       timeSteps = timeSteps,
                       opts = opts,
                       writeMcAll = writeMcAll,
                       compress = compress,
                       misc = misc,
                       thermalAvailabilities = thermalAvailabilities,
                       hydroStorage = hydroStorage,
                       hydroStorageMaxPower = hydroStorageMaxPower,
                       reserve = reserve,
                       linkCapacity = linkCapacity,
                       mustRun = mustRun,
                       thermalModulation = thermalModulation,
                       removeVirtualAreas = removeVirtualAreas,
                       storageFlexibility = storageFlexibility,
                       production = production,
                       reassignCosts = reassignCosts,
                       newCols = newCols)
  }else{
    studieSToWrite <- list.dirs(paste0(opts$studyPath, "/output"), recursive = FALSE, full.names = FALSE)
    studieSToWrite <- setdiff(studieSToWrite, "maps")
    if(length(studieSToWrite) > 0){
      studyPath <- opts$studyPath
      if(nbCores>1)
      {
        cl <- makeCluster(nbCores)
        clusterEvalQ(cl, {
          library(antaresRead)
        })
        clusterExport(cl, c("path","opts","studyPath",
                            "timeSteps",
                            "writeMcAll",
                            "compress",
                            "misc",
                            "thermalAvailabilities",
                            "hydroStorage",
                            "hydroStorageMaxPower",
                            "reserve",
                            "linkCapacity",
                            "mustRun",
                            "thermalModulation",
                            "removeVirtualAreas",
                            "storageFlexibility",
                            "production",
                            "reassignCosts",
                            "newCols", 
                            "overwrite",
                            ".writeAntaresH5Fun"
        ), envir = environment())
        
        parSapplyLB(cl, studieSToWrite, function(X){
          opts <- setSimulationPath(studyPath, X)
          if(!is.null(path)){
            pathStud <- paste0(path, "/", X, ".h5")
          }
          
          if(overwrite & file.exists(pathStud)){
            file.remove(pathStud)
          }
          
          if(file.exists(pathStud)){
            stop(paste0("File ", pathStud, " already exist you must use overwrite argument if you want to overwrite"))
          }
          
          
          .writeAntaresH5Fun(path = pathStud,
                                           timeSteps = timeSteps,
                                           opts = opts,
                                           writeMcAll = writeMcAll,
                                           compress = compress,
                                           misc = misc,
                                           thermalAvailabilities = thermalAvailabilities,
                                           hydroStorage = hydroStorage,
                                           hydroStorageMaxPower = hydroStorageMaxPower,
                                           reserve = reserve,
                                           linkCapacity = linkCapacity,
                                           mustRun = mustRun,
                                           thermalModulation = thermalModulation,
                                           removeVirtualAreas = removeVirtualAreas,
                                           storageFlexibility = storageFlexibility,
                                           production = production,
                                           reassignCosts = reassignCosts,
                                           newCols = newCols)
          
          
        })
        stopCluster(cl)
        
      }else{
        sapply(studieSToWrite, function(X){
          opts <- setSimulationPath(studyPath, X)
          if(!is.null(path)){
            pathStud <- paste0(path, "/", opts$studyName, ".h5")
          }
          
          if(overwrite & file.exists(pathStud)){
            file.remove(pathStud)
          }
          if(file.exists(pathStud)){
            stop(paste0("File ", pathStud, " already exist you must use overwrite argument if you want to overwrite"))
          }
          
          .writeAntaresH5Fun(path = pathStud,
                             timeSteps = timeSteps,
                             opts = opts,
                             writeMcAll = writeMcAll,
                             compress = compress,
                             misc = misc,
                             thermalAvailabilities = thermalAvailabilities,
                             hydroStorage = hydroStorage,
                             hydroStorageMaxPower = hydroStorageMaxPower,
                             reserve = reserve,
                             linkCapacity = linkCapacity,
                             mustRun = mustRun,
                             thermalModulation = thermalModulation,
                             removeVirtualAreas = removeVirtualAreas,
                             storageFlexibility = storageFlexibility,
                             production = production,
                             reassignCosts = reassignCosts,
                             newCols = newCols)
          
          
        })
        
      }
    } else {
      message("No study.")
    }
  }
  
  
  
}

#' Convert antares output to h5 file
#'
#' @export
#' @noRd
.writeAntaresH5Fun <- function(path,
                               timeSteps,
                               opts,
                               writeMcAll,
                               compress,
                               misc,
                               thermalAvailabilities,
                               hydroStorage,
                               hydroStorageMaxPower,
                               reserve,
                               linkCapacity,
                               mustRun,
                               thermalModulation,
                               removeVirtualAreas,
                               storageFlexibility,
                               production,
                               reassignCosts,
                               newCols){
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  if(is.null(path)){
    studPath <- unlist(strsplit(opts$simPath, "/"))
    studName <- studPath[length(studPath)]
    path <- paste0(studName, ".h5")
  }
  
  #Create h5 file
  rhdf5::h5createFile(path)
  
  #loop on timeStep
  sapply(timeSteps, function(timeStep){
    
    #Add mcAll
    allMcYears <- opts$mcYears
    if(writeMcAll){
      allMcYears <- c(allMcYears, -1)
    }
    
    #Loop on MCyear
    sapply(allMcYears, function(mcY)
    {
      if(allMcYears[1] == mcY){
        writeStructure = TRUE
      }else{
        writeStructure = FALSE
      }
      mcAll <- FALSE
      if(mcY == -1){
        mcY <- NULL
        writeStructure <- TRUE
        mcAll <- TRUE
      }
      
      #Read data
      res <- readAntares(areas = "all" ,
                         links = "all",
                         clusters = "all",
                         districts = "all",
                         mcYears = mcY,
                         timeStep = timeStep, opts = opts, showProgress = FALSE,
                         misc = misc, thermalAvailabilities = thermalAvailabilities,
                         hydroStorage = hydroStorage, hydroStorageMaxPower = hydroStorageMaxPower,
                         reserve = reserve, linkCapacity = linkCapacity, mustRun = mustRun,
                         thermalModulation = thermalModulation)
      
      if(removeVirtualAreas){
        res <- removeVirtualAreas(res,
                           storageFlexibility = storageFlexibility,
                           production = production,
                           reassignCosts = reassignCosts,
                           newCols = newCols)
      }
      
      if(writeStructure & !mcAll){
        
        
        # Create group
        rhdf5::H5close()
        rhdf5::h5createGroup(path, timeStep)
        rhdf5::H5close()
        #Write time
        writeTime(res, path, timeStep)
        rhdf5::H5close()
        #Write attributes
        
        attrib <- attributes(res)
        s <- serialize(attrib, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/attrib"))
        
        # .writeAttributes(res = res, path = path, timeStep = timeStep)
        
        
        
        ###Write inputs
        rhdf5::h5createGroup(path, paste0(timeStep, "/inputs"))
        layout <- readLayout()
        s <- serialize(layout, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/inputs/layout"))
        cldesc <- readClusterDesc()
        s <- serialize(cldesc, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/inputs/cldesc"))
        bc <- readBindingConstraints()
        s <- serialize(bc, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/inputs/buildingcte"))
        
      }
      
      #Remove useless data
      ctrl <- sapply(1:length(res), function(i){
        if("day" %in% names(res[[i]])){
          res[[i]][, "day" := NULL]
        }
        if("month" %in% names(res[[i]])){
          res[[i]][, "month" := NULL]
        }
        if("hour" %in% names(res[[i]])){
          res[[i]][, "hour" := NULL]
        }
        if("time" %in% names(res[[i]])){
          res[[i]][, "time" := NULL]
        }
        invisible()
      })
      gc()
      
      
      if(is.null(mcY)){
        
        lapply(res, function(X){
          X[, "mcYear" := "mcAll"]
          
        })
      }
      #Transform for write
      res <- transformH5(res,areasKey = c("area", "mcYear"),
                         linksKey = c("link",  "mcYear"),
                         districtKey = c("district",  "mcYear"),
                         clustersKey = c("area", "cluster",  "mcYear"))
      #Write data
      writeAntaresData(res, path, timeStep, writeStructure, mcAll, compress)
    })
  })
  rhdf5::H5close()
  message(paste0("File .h5 writed : ", path, "\n"))
  invisible()
}
