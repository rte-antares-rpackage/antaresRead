#' Convert antares output to h5 file
#'
#' @param path \code{character} folder where h5 file will be write (default NULL)
#' @param timeSteps \code{character} timeSteps
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Default to \code{antaresRead::simOptions()}
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
#' @param allData \code{boolean} add all data with a single call (writeMcAll, misc, thermalAvailabilities, hydroStorage, hydroStorageMaxPower
#' reserve, linkCapacity, mustRun, thermalModulation).
#' @param writeAllSimulations \code{boolean}, write all simulations of your antares study.
#' @param nbCores \code{numeric}, number of cores to use, only used if writeAllSimulations is TRUE
#' @param removeVirtualAreas \code{boolean}, remove virtual areas, see \link[antaresRead]{removeVirtualAreas}
#' @param storageFlexibility \code{character or list}, see \link[antaresRead]{removeVirtualAreas}
#' @param production \code{character or list}, see \link[antaresRead]{removeVirtualAreas}
#' @param reassignCosts \code{boolean or list}, see \link[antaresRead]{removeVirtualAreas}
#' @param newCols \code{boolean or list}, see \link[antaresRead]{removeVirtualAreas}
#' @param overwrite \code{boolean or list}, overwrite old file
#' @param supressMessages \code{boolean}, supress messages from \link[antaresRead]{readAntares} and \link[antaresRead]{removeVirtualAreas}
#' 
#' @examples
#' 
#' \dontrun{
#' # Write simulation one by one
#' setSimulationPath("C:/Users/MyUser/Mystudy", 1)
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY")
#'
#' # Write all simulations
#' setSimulationPath("C:/Users/MyUser/Mystudy")
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY", writeAllSimulations = TRUE)
#'
#' # Choose timestep to write
#' setSimulationPath("C:/Users/MyUser/Mystudy", 1)
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY", timeSteps = "hourly")
#'
#' # Write with additionnal information
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY", timeSteps = "hourly",
#'    misc = TRUE, thermalAvailabilities = TRUE,
#'    hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
#'    linkCapacity = TRUE, mustRun = TRUE, thermalModulation = TRUE)
#' 
#' # Write all data with a shorcut 
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY", allData = TRUE)
#' 
#' #Remove virtuals areas
#' 
#' writeAntaresH5(path="PATH_TO_YOUR_STUDY", timeSteps = "hourly", overwrite = TRUE,
#'                writeMcAll = FALSE, removeVirtualAreas = TRUE,
#'                storageFlexibility =  "psp in-2",
#'                production =  NULL, reassignCosts =FALSE, newCols = TRUE)
#'                
#' #Remove virtuals areas more than one call
#' writeAntaresH5(
#'                path="PATH_TO_YOUR_STUDY", 
#'                timeSteps = "hourly", 
#'                overwrite = TRUE,
#'                writeMcAll = FALSE, 
#'                removeVirtualAreas = TRUE, 
#'                storageFlexibility = list("psp out", "psp in-2"),
#'                production = list(NULL, NULL), 
#'                reassignCosts = list(TRUE, FALSE), 
#'                newCols = list(FALSE, TRUE)
#'                )
#'
#'
#' }
#' @export
writeAntaresH5 <- function(path = NULL, timeSteps = c("hourly", "daily", "weekly", "monthly", "annual"),
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
                           allData = FALSE,
                           writeAllSimulations = FALSE,
                           nbCores = 4,
                           removeVirtualAreas = FALSE,
                           storageFlexibility = NULL,
                           production = NULL,
                           reassignCosts = FALSE,
                           newCols = TRUE, 
                           overwrite = FALSE, supressMessages = FALSE){
  
  if(!dir.exists(path)){
    stop(paste0("Folder ", path, " not found."))
  }
  
  if(allData){
    writeMcAll <- TRUE
    misc <- TRUE
    thermalAvailabilities <- TRUE
    hydroStorage <- TRUE
    hydroStorageMaxPower <- TRUE
    reserve <- TRUE
    linkCapacity <- TRUE
    mustRun <- TRUE
    thermalModulation <- TRUE
  }
  
  .requireRhdf5_Antares()
  
  rhdf5::h5closeAll()
  
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
                       newCols = newCols,
                       supressMessages = supressMessages)
  }else{
    studieSToWrite <- list.dirs(paste0(opts$studyPath, "/output"), recursive = FALSE, full.names = FALSE)
    studieSToWrite <- setdiff(studieSToWrite, "maps")
    if(length(studieSToWrite) > 0){
      studyPath <- opts$studyPath
      if(nbCores>1)
      {
        if(!requireNamespace("parallel")) stop("Error loading 'parallel' package.")
        
        cl <- parallel::makeCluster(nbCores)
        parallel::clusterEvalQ(cl, {
          library(antaresRead)
        })
        parallel::clusterExport(cl, c("path","opts","studyPath",
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
                                      ".writeAntaresH5Fun",
                                      "supressMessages"
        ), envir = environment())
        
        parallel::parSapplyLB(cl, studieSToWrite, function(X){
          if(supressMessages)
          {
            opts <- suppressWarnings(suppressMessages(setSimulationPath(studyPath, X)))
          }else{
            opts <- setSimulationPath(studyPath, X)
          }
          
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
                             newCols = newCols,
                             supressMessages = supressMessages)
          
          
        })
        parallel::stopCluster(cl)
        
      }else{
        sapply(studieSToWrite, function(X){
          if(supressMessages)
          {
            opts <- suppressWarnings(suppressMessages(setSimulationPath(studyPath, X)))
          }else{
            opts <- setSimulationPath(studyPath, X)
          }
          
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
                             newCols = newCols,
                             supressMessages = supressMessages)
          
          
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
                               newCols,
                               supressMessages){
  
  .requireRhdf5_Antares()
  
  
  if(is.null(path)){
    studPath <- unlist(strsplit(opts$simPath, "/"))
    studName <- studPath[length(studPath)]
    path <- paste0(studName, ".h5")
  }
  
  #Close connection if exist
  rhdf5::h5closeAll()
  
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
      
      messageS <- ifelse(allMcYears[1] == mcY & timeSteps[1] == timeStep && !supressMessages, TRUE, FALSE)
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
      if(messageS){
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
        
        
      }else{
        res <- suppressWarnings(suppressMessages(readAntares(areas = "all" ,
                                                             links = "all",
                                                             clusters = "all",
                                                             districts = "all",
                                                             mcYears = mcY,
                                                             timeStep = timeStep, opts = opts, showProgress = FALSE,
                                                             misc = misc, thermalAvailabilities = thermalAvailabilities,
                                                             hydroStorage = hydroStorage, hydroStorageMaxPower = hydroStorageMaxPower,
                                                             reserve = reserve, linkCapacity = linkCapacity, mustRun = mustRun,
                                                             thermalModulation = thermalModulation)))
        
      }
      
      if(removeVirtualAreas){
        if(!(is.list(storageFlexibility)))
        {
          if(messageS){
            res <- removeVirtualAreas(res,
                                      storageFlexibility = storageFlexibility,
                                      production = production,
                                      reassignCosts = reassignCosts,
                                      newCols = newCols)
          }else{
            res <-  suppressWarnings(suppressMessages(removeVirtualAreas(res,
                                                                         storageFlexibility = storageFlexibility,
                                                                         production = production,
                                                                         reassignCosts = reassignCosts,
                                                                         newCols = newCols)))
          }
        }else{
          
          for(i in 1:length(storageFlexibility)){
            res <-  suppressWarnings(
              suppressMessages(
                removeVirtualAreas(res,
                                   storageFlexibility = storageFlexibility[[i]],
                                   production = production[[i]],
                                   reassignCosts = reassignCosts[[i]],
                                   newCols = newCols[[i]])))
          }
          
        }
      }
      
      if(writeStructure & !mcAll){
        
        
        # Create group
        rhdf5::h5closeAll()
        rhdf5::h5createGroup(path, timeStep)
        rhdf5::h5closeAll()
        #Write time
        writeTime(res, path, timeStep)
        rhdf5::h5closeAll()
        #Write attributes
        
        attrib <- attributes(res)
        s <- serialize(attrib, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/attrib"))
        
        # .writeAttributes(res = res, path = path, timeStep = timeStep)
        
        
        
        ###Write inputs
        rhdf5::h5createGroup(path, paste0(timeStep, "/inputs"))
        if(messageS){
          layout <- readLayout()
        }else{
          layout <- suppressWarnings(suppressMessages(readLayout()))
        }
        s <- serialize(layout, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/inputs/layout"))
        
        if(messageS){
          cldesc <- readClusterDesc()
        }else{
          cldesc <- suppressWarnings(suppressMessages(readClusterDesc()))
        }
        s <- serialize(cldesc, NULL, ascii = TRUE)
        rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/inputs/cldesc"))
        if(messageS){
          bc <- readBindingConstraints()
        }else{
          bc <- suppressWarnings(suppressMessages(readBindingConstraints()))
        }
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
  rhdf5::h5closeAll()
  message(paste0("File .h5 writed : ", path, "\n"))
  invisible()
}
