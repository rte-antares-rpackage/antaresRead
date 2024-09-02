#' Creation of Mc_all new (only antares > V6)
#'
#' Creation of Mc_all new (only antares > V6) 
#'
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param nbcl \code{numeric} Number of parralel process
#' @param verbose \code{numeric} show log in console. Defaut to 1  
#'  - 0 : No log  
#'  - 1 : Short log  
#'  - 2 : Long log
#' @param timestep \code{character} antares timestep
#' @param writeOutput \code{boolean} write result or not.
#' @param mcWeights \code{numeric} vector of weigth for mcYears.
#' @param mcYears  \code{numeric} mcYears to load.
#' @param filtering \code{boolean} filtering control
#' @param selected \code{list} named list (pass to antaresRead) : list(areas = 'a', links = 'a - e')
#' @param legacy \code{boolean} run old version of the function
#'
#' @import data.table
#' @import doParallel
#' @importFrom plyr llply
#' @importFrom stringr str_split
#' @importFrom stringi stri_replace_last_fixed
#' @importFrom memuse Sys.meminfo
#' 
#' @return Object `list` of data.tables, each element representing one type 
#' of element (areas, links, clusters)
#'
#' @export
#' 
#' @rdname aggregatate_mc_all
#' 
parAggregateMCall <- function(opts, 
                              nbcl = 8, 
                              verbose = 2, 
                              timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
                              writeOutput = TRUE, # for ADPatch compatibility
                              mcWeights = NULL,
                              mcYears = NULL,
                              filtering = FALSE,
                              selected = NULL,
                              legacy = FALSE){
  
  total_time <- Sys.time()
  
  if (isTRUE(legacy)) {
    cat("\nRunning Legacy mode\n")
    parAggregateMCall_old(opts = opts, nbcl = nbcl, verbose = verbose,
                          timestep = timestep, writeOutput = writeOutput,
                          mcWeights = mcWeights,
                          mcYears = NULL)
    return (2)
  }
  
  # browser()
  
  resultat <- list()
  
  #Get all available timestep####
  firstYear_folder <- list.dirs(file.path(opts$simDataPath,"mc-ind"), recursive = F)[1]
  timestep_dispo <- grep(".txt", list.files(firstYear_folder, recursive = T), value = T)
  timestep_dispo <- unique(gsub(".*-(.+)\\..*", "\\1", timestep_dispo))
  timestep <- intersect(timestep, timestep_dispo)
  
  if (verbose > 0) print(timestep)
  if (is.null(timestep)){
    cat("No data found")
    return (1)  
  }
  
  # Renommer le dossier mc_all si exist####
  if (writeOutput && dir.exists(file.path(opts$simDataPath,"mc-all"))){
    if (dir.exists(file.path(opts$simDataPath, "original-mc-all"))){
      unlink(file.path(opts$simDataPath,"mc-all"), recursive = T)
    } else {
      file.rename(file.path(opts$simDataPath,"mc-all"), file.path(opts$simDataPath,"original-mc-all"))
    }
    setSimulationPath(opts$simPath)
  }
  
  for (tmstp in timestep){
    closeAllConnections()
    pathEtude <- opts$simPath
    parallel <- (nbcl > 1)
    
    Folder <- Files <- Mode <- Name <- progNam <- `production EXP` <- `NODU EXP`<- `NP Cost EXP` <- `production` <- `NODU`<- `NP Cost` <- NULL
    # opts
    # verbose = 1
    # filtering = FALSE
    # selected = NULL
    # timestep = "houry"
    # writeOutput = FALSE
    # mcWeights = c(1, 2)
    # mcYears = c(1, 3)
    
    if(writeOutput == FALSE){ coef = 1.4 } else { coef = 1 }
    if(writeOutput == FALSE & length(tmstp)>1) stop("If you want data return you must choose a unique timestep")
    
    if(verbose > 0) try ({pb <- txtProgressBar(style = 3)})
    
    opts <- suppressWarnings(setSimulationPath(opts$simPath))
    
    # Version which readAntares####
    RES_mode <- F
    if (opts$antaresVersion >= 810) 
      if (opts$parameters$`other preferences`$`renewable-generation-modelling` == "clusters")
        RES_mode <- T
    linkTable_file <- ifelse(RES_mode, "/format_output/tableOutput_aggreg_v8.csv", "/format_output/tableOutput_aggreg.csv")
    
    linkTable <- try({
      data.table::fread(system.file(linkTable_file, package = "antaresRead"))},
      silent = TRUE
    )
    .errorTest(linkTable, verbose, "\nLoad of link table")
    
    # load link table####
    linkTable$progNam <- linkTable$Stats
    linkTable$progNam[which(linkTable$progNam == "values")] <- "EXP"
    # dtaMc <- paste0(opts$simDataPath, "/mc-ind")
    if(!is.null(mcYears)){
      numMc <- mcYears
      if(length(numMc) == 1) if(numMc%in% c("all", "All")) numMc <- opts$mcYears
      
    } else numMc <- opts$mcYears
    
    if(is.null(mcWeights)) mcWeights <- rep(1, length(numMc))
    if(length(mcWeights)!=length(numMc)) stop('length of mcWeights must be the same as mcYears')
    
    coef_div_mc_pond <- sum(mcWeights)
    coef_div_mc_pond_2 <- sum(mcWeights * mcWeights)
    
    #sapply on timeStep
    #allTyped <- c("annual", "daily", "hourly", "monthly", "weekly")
    #allTyped <- 'hourly'
    
    gc()
    #Dynamic batch value####
    batch = floor(((as.numeric(Sys.meminfo()[[2]])/(1024*1024*1024)) * 0.7)/2)
    if (batch > 1 & length(numMc)%%batch == 1) batch <- batch + 1
    if (verbose > 0) cat("\nBatch :",batch,"\n")
    
    #Parallel config####
    if (parallel){
      closeAllConnections()
      cl <- makeCluster(nbcl)
      registerDoParallel(cl)
      paropts <- list(preschedule=TRUE)
      clusterSetRNGStream(cl, 123)
    }
    
    #Start mc-all calculation####
    output <- sapply(tmstp, function(type, verbose)
    {
      
      .addMessage(verbose, paste0("------- Mc-all : ", type, " -------"))
      
      try({
        #load first MC-year####
        a <- Sys.time()
        oldw <- getOption("warn")
        
        if(!filtering)
        {
          clustersSelect <- clustersResSelect <- areasselect <- linksSelect <- "all"
        } else {
          if(is.null(selected)){
            areasselect <- .getAreasToAggregate(opts, type)
            linksSelect <- .getLinksToAggregate(opts, type)
            clustersSelect <- clustersResSelect <- areasselect
          } else {
            areasselect <- selected$areas
            linksSelect <- selected$links
            clustersSelect <- selected[["clusters"]]
            clustersResSelect <- selected[["clustersRes"]]
          }
        }
        #get image of memory before and after reading one mcYear
        #tt <- sum(.Internal(gc(FALSE, TRUE, TRUE))[13:14])
        dta <- suppressWarnings(readAntares(areas = areasselect,
                                            links = linksSelect,
                                            clusters = clustersSelect,
                                            clustersRes = clustersResSelect,
                                            timeStep = type,
                                            simplify = FALSE,
                                            mcYears = numMc[1],
                                            showProgress = FALSE))
        #tt = sum(.Internal(gc(FALSE, FALSE, TRUE))[13:14]) - tt
        #print(tt, units = "Mb")
        
        if(length(dta)>0){
          dtaLoadAndcalcul <- try({
            aTot <- as.numeric(Sys.time() - a)
            
            SDcolsStartareas <- switch(type,
                                       daily = 6,
                                       annual = 4,
                                       hourly = 7,
                                       monthly = 5,
                                       weekly = 4
            )
            
            SDcolsStartClust <-  SDcolsStartareas + 1
            
            #make structure####
            struct <- list()
            for (elmt in c("areas","links","clusters","clustersRes")){
              if (!is.null(dta[[elmt]]))
                struct[[elmt]] <- dta[[elmt]][,.SD, .SDcols = 1:ifelse(elmt %in% c("areas","links"), SDcolsStartareas, SDcolsStartClust)]
              
              if (!is.null(struct[[elmt]]$day))
                struct[[elmt]]$day <- sprintf(struct[[elmt]]$day, fmt = "%02.0f")
              
              if (type == "weekly"){
                if (!is.null(struct[[elmt]]))
                  struct[[elmt]]$timeId <- as.numeric(substr(struct[[elmt]]$time, 
                                                             nchar(as.character(struct[[elmt]]$time[1]))-1,
                                                             nchar(as.character(struct[[elmt]]$time[1]))))
              }
            }
            b <- Sys.time()
            
            #value structure####
            value <- .giveValue(dta, SDcolsStartareas, SDcolsStartClust)
            N <- length(numMc)
            W_sum = w_sum2 = mean_m = S = 0
            
            value <- lapply(value, function(X){.creatStats(X, W_sum, w_sum2, mean_m, S, mcWeights[1])})
            
            btot <- as.numeric(Sys.time() - b)
            if(verbose>0) try({.progBar(pb, type, 1, N, coef)})
            
            #Record years####
            recordYears <- .createRecordYears(value, numMc[1])
            
            #LOLD_data####
            LOLD_data <- data.table(area = unique(dta$areas[, area]), isLOLD_cum = 0)
            LOLD_data[area %in% unique(dta$areas[LOLD > 0, area]), isLOLD_cum := isLOLD_cum + mcWeights[numMc == numMc[1]]]
            
            #sequentially add values####
            if(N>1)
            {
              for (j in 1:ceiling(N/batch)){
                lst_idx = 0 
                left = max((j-1)*batch + 1, 2)
                right = min(N, j*batch)
                curr_years = setdiff(numMc[left:right],NA)
                lst_dtaTP <- list()                 #init lst pour execution monocore
                
                if (parallel){
                  par_time <- Sys.time()
                  lst_dtaTP <- suppressWarnings(plyr::llply(curr_years, .parReadAntares, .parallel = T, 
                                                            .paropts = list(.options.snow = paropts), 
                                                            pth = opts$simPath, type = type, areasselect = areasselect,
                                                            linksSelect = linksSelect, clustersSelect = clustersSelect,
                                                            clustersResSelect = clustersResSelect))
                  if (verbose > 0) cat("\n\n",Sys.time() - par_time)
                } 
                
                for(i in curr_years){
                  lst_idx = lst_idx + 1
                  a <- Sys.time()
                  
                  if (!parallel){
                    dtaTP <- suppressWarnings(readAntares(areas = areasselect,
                                                          links = linksSelect,
                                                          clusters = clustersSelect,
                                                          clustersRes = clustersResSelect,
                                                          timeStep = type,
                                                          simplify = FALSE,
                                                          mcYears = i,
                                                          showProgress = FALSE))
                  } else { dtaTP <- lst_dtaTP[[lst_idx]]}
                  
                  aTot <- aTot + as.numeric(Sys.time() - a)
                  b <- Sys.time()
                  
                  valueTP <- .giveValue(dtaTP, SDcolsStartareas, SDcolsStartClust)
                  
                  nmKeep <- names(valueTP)
                  
                  valueTP <- lapply(names(valueTP), function(X){
                    .creatStats(valueTP[[X]], value[[X]]$W_sum, value[[X]]$w_sum2, value[[X]]$mean_m, value[[X]]$S , mcWeights[numMc == i])
                  })
                  
                  names(valueTP) <- nmKeep 
                  
                  #LOLP and record years
                  LOLD_data[area %in% unique(dtaTP$areas[LOLD > 0, area]), isLOLD_cum := isLOLD_cum + mcWeights[numMc == i]]
                  recordYears <- .updateRecordYears(recordYears, value, valueTP, i)
                  
                  # valueTP <- mapply(function(X, Y){.creatStats(X, Y$W_sum, Y$w_sum2, Y$mean_m, Y$S , mcWeights[i])}, X = valueTP, Y = value, SIMPLIFY = FALSE)
                  
                  for (elmt in c("areas","links","clusters","clustersRes")){
                    if (!is.null(value[[elmt]]))
                      value[[elmt]] <- .updateStats(value[[elmt]], valueTP[[elmt]])
                  }
                  
                  btot <- btot + as.numeric(Sys.time() - b)
                  if(verbose>0) try({.progBar(pb, type, i, N, coef)})
                  
                  #free memory after each iteration####
                  lst_dtaTP[[lst_idx]] <- 0
                }
              }
              
              #Calcul of sd
              b <- Sys.time()
              
              coef_div_var = (coef_div_mc_pond )#- coef_div_mc_pond_2 / coef_div_mc_pond
              
              for (elmt in c("areas","links","clusters","clustersRes")){
                if (!is.null(value[[elmt]])){
                  suppressWarnings(
                    value[[elmt]]$std <- sqrt(value[[elmt]]$var / coef_div_var)
                  )
                  #nan due to round
                  for (i in names(value[[elmt]]$std))
                    value[[elmt]]$std[is.nan(get(i)), (i) := 0]
                }
              }
            } else {
              # std to 0
              value <- lapply(value, function(x){
                if(!is.null(x$sumC)){
                  x$std <- x$sumC
                  x$std[, c(colnames(x$std)) := lapply(.SD, function(x) 0), .SDcols = colnames(x$std)]
                  colnames(x$std) <- gsub("_std$", "", colnames(x$std))
                  x
                }
              })
            }
            
            for (itm in names(value)){
              if(!is.null(value[[itm]])){
                value[[itm]]$sumC <- NULL
                value[[itm]]$var <- NULL
                value[[itm]]$S <- NULL
                value[[itm]]$W_sum <- NULL
                value[[itm]]$w_sum2 <- NULL
                value[[itm]]$mean_m <- NULL
                if (!is.null(names(value[[itm]]$std))){
                  names(value[[itm]]$std) <- paste0(names(value[[itm]]$std) , "_std")
                }
                value[[itm]]$sum <- value[[itm]]$sum / coef_div_mc_pond
              }
            }
            
            btot <- btot + as.numeric(Sys.time() - b)
            .addMessage(verbose, paste0("\nTime for reading data : ", round(aTot,1), " secondes"))
            .addMessage(verbose, paste0("Time for calculating : ", round(btot,1), " secondes"))
          }, silent = TRUE)
          
          .errorTest(dtaLoadAndcalcul, verbose, "Load data and calcul")
          
          # browser()
          
          #write mc-all files####
          allfiles <- c("values", "id")
          
          if(writeOutput == FALSE){
            if(verbose>0) .progBar(pb, type, 1, 1, 1, terminate = TRUE)
            return(.formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct))
            
          } else {
            if(!is.null(value$clustersRes) && is.data.frame(value$clustersRes) && nrow(value$clustersRes) > 0){
              warning("Writing clusterRes file is not at moment available")
            }
            
            #write areas + record years####
            areaWrite <- try(sapply(allfiles, function(f)
            {
              #prepare data for all country
              areaSpecialFile <- linkTable[Folder == "area" & 
                                             Files == "values" & Mode == tolower(opts$mode)]
              
              if (f == "id") areaSpecialFile <- areaSpecialFile[Stats %in% c("min","max")]
              #mc-all variables !
              mcall_vars_area <- paste0(areaSpecialFile$Name, "_", areaSpecialFile$progNam)
              mcall_vars_area <- gsub("_EXP", "", mcall_vars_area)
              if (f == "values")
                areas <- cbind(value$areas$sum, value$areas$std, value$areas$min, value$areas$max)
              else
                areas <- cbind(recordYears$areas$min, recordYears$areas$max)
              if(nrow(areas) > 0)
              {
                # areas <- areas[, .SD, .SDcols = which(names(areas)%in%opts$variables$areas)]
                # areas <- areas[, .SD, .SDcols = match(opts$variables$areas, names(areas))]
                areas <- areas[, .SD, .SDcols = intersect(mcall_vars_area,names(areas))]
                
                nbvar <- ncol(areas)
                areas <- cbind(struct$areas, areas)
                ncolFix <- ncol(struct$areas) - 3
                areas[, c("mcYear", "time") := NULL]
                allAreas <- unique(areas$area)
                
                if (f == "values")
                  for(i in 1:length(mcall_vars_area))
                  {
                    var <- mcall_vars_area[i]
                    dig <- areaSpecialFile[Name == var | paste0(Name,"_",progNam) == var]$digits
                    if(length(dig)>0)areas[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
                  }
                
                if(length(allAreas) > 0)
                {
                  sapply(allAreas,  function(areasel){
                    #for each country prepare file
                    areastowrite <- areas[area == areasel]
                    areastowrite[,c("area") := NULL]
                    indexMin <- min(areas$timeId)
                    indexMax <- max(areas$timeId)
                    kepNam <- names(struct$areas)[!names(struct$areas)%in%c("area","mcYear","time")]
                    nameIndex <- ifelse(type == "weekly", "week", "index")
                    kepNam[which(kepNam == "timeId")] <- nameIndex
                    #write txt
                    .writeFileOut(dta = areastowrite, timestep = type, fileType = f,
                                  ctry = areasel, opts = opts, folderType = "areas", nbvar = nbvar,
                                  indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                  nomcair = areaSpecialFile$Name, unit = areaSpecialFile$Unit,
                                  nomStruct = kepNam,Stats = areaSpecialFile$Stats)
                    
                    
                  })
                }
              }
            }), silent = TRUE)
            
            .errorTest(areaWrite, verbose, "Area (+ record years) write")
            
            allfiles <- c("values", "id")
            #write links + record years ####
            linkWrite <- try(sapply(allfiles, function(f)
            {
              #prepare data for all link
              linkSpecialFile <- linkTable[Folder == "link" & 
                                             Files == "values" & Mode == tolower(opts$mode)]
              if (f == "id") linkSpecialFile <- linkSpecialFile[Stats %in% c("min","max")]
              #mc-all variables !
              mcall_vars_links <- paste0(linkSpecialFile$Name, "_", linkSpecialFile$progNam)
              mcall_vars_links <- gsub("_EXP", "", mcall_vars_links)
              if (f == "values")
                links <- cbind(value$links$sum,  value$links$std, value$links$min, value$links$max)
              else
                links <- cbind(recordYears$links$min, recordYears$links$max)
              if(nrow(links) > 0)
              {
                
                links <- links[, .SD, .SDcols = intersect(mcall_vars_links,names(links))]
                # links <- links[, .SD, .SDcols = which(names(links)%in%opts$variables$links)]
                # links <- links[, .SD, .SDcols = match(opts$variables$links, names(links))]
                
                nbvar <- ncol(links)
                links <- cbind(struct$links, links)
                ncolFix <- ncol(struct$links)-3
                links[, c("mcYear", "time") := NULL]
                allLink<- unique(links$link)
                
                if (f == "values"){
                  for(i in 1:length(mcall_vars_links))
                  {
                    var <- mcall_vars_links[i]
                    dig <- linkSpecialFile[Name == var | paste0(Name,"_",progNam) == var]$digits
                    if(length(dig)>0)links[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
                  }
                }
                
                sapply(allLink,  function(linksel){
                  #for eatch link prepare file
                  linkstowrite <- links[link == linksel]
                  linkstowrite[,c("link") := NULL]
                  indexMin <- min(links$timeId)
                  indexMax <- max(links$timeId)
                  kepNam <- names(struct$link)[!names(struct$link)%in%c("link","mcYear","time")]
                  nameIndex <- ifelse(type == "weekly", "week", "index")
                  kepNam[which(kepNam == "timeId")] <- nameIndex
                  #write txt
                  .writeFileOut(dta = linkstowrite, timestep = type, fileType = f,
                                ctry = linksel, opts = opts, folderType = "links", nbvar = nbvar,
                                indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                nomcair = linkSpecialFile$Name, unit = linkSpecialFile$Unit,
                                nomStruct = kepNam,Stats = linkSpecialFile$Stats)
                })
              }
            }), silent = TRUE)
            
            .errorTest(linkWrite, verbose, "Link (+ record years) write")
            
            #write details####
            if (!is.null(value$clustersRes)){
              details <- value$clusters$sum
              if(!is.null(struct$clusters$day) | 1)
              {
                if(length(struct$clusters$day) > 0 | 1)
                {
                  endClust <- cbind(struct$clusters, details)
                  endClust[, c("mcYear") := NULL]
                  detailWrite <- try(sapply(unique(endClust$area),  function(ctry){
                    #for each country prepare file
                    endClustctry <- endClust[area == ctry]
                    orderBeg <- unique(endClustctry$time)
                    endClustctry[,c("area") := NULL]
                    
                    nameBy <- setdiff(names(endClustctry), names(struct$clusters))
                    # if("NP Cost"%in%names(endClustctry)){}
                    nomStruct <- names(endClustctry)[!names(endClustctry) %in% c("cluster", nameBy)]
                    
                    tmp_formula <- nomStruct
                    # tmp_formula <- gsub(" ", "_", tmp_formula)
                    tmp_formula <- paste0("`", tmp_formula, "`")
                    
                    tmp_formula <- as.formula(paste0(paste0(tmp_formula, collapse = " + "), "~cluster"))
                    
                    endClustctry[, c(nameBy) := lapply(.SD, round), .SDcols = nameBy]
                    
                    endClustctry <- data.table::dcast(endClustctry, tmp_formula,
                                                      value.var = c(nameBy))
                    
                    endClustctry <- endClustctry[match(orderBeg, endClustctry$time)]
                    endClustctry[,c("time") := NULL]
                    nomStruct <- nomStruct[-which(nomStruct == "time")]
                    nomcair <- names(endClustctry)
                    nomcair <- nomcair[!nomcair%in%nomStruct]
                    nbvar <- length(nomcair)
                    unit <- rep("", length(nomcair))
                    unit[grep("production",nomcair)] <- "MWh"
                    unit[grep("NP Cost",nomcair)] <- "NP Cost - Euro"
                    unit[grep("NODU",nomcair)] <- "NODU"
                    unit[grep("profit",nomcair)] <- "Profit - Euro"
                    nomcair <- gsub("production_","",nomcair)
                    nomcair <- gsub("NP Cost_","",nomcair)
                    nomcair <- gsub("NODU_","",nomcair)
                    nomcair <- gsub("profit_","",nomcair)
                    Stats <- rep("EXP", length(unit))
                    nameIndex <- ifelse(type == "weekly", "week", "index")
                    nomStruct[which(nomStruct == "timeId")] <- nameIndex
                    indexMin <- min(endClustctry$timeId)
                    indexMax <- max(endClustctry$timeId)
                    ncolFix <- length(nomStruct)
                    #write details txt
                    .writeFileOut(dta = endClustctry, timestep = type, fileType = "details",
                                  ctry = ctry, opts = opts, folderType = "areas", nbvar = nbvar,
                                  indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                  nomcair = nomcair, unit = unit, nomStruct = nomStruct,Stats = Stats)
                  }), silent = TRUE)
                  .errorTest(detailWrite, verbose, "Detail write")
                }
              }
            }
            
            #write details-res####
            if (!is.null(value$clustersRes)){
              detailsRes <- value$clustersRes$sum
              
              if(!is.null(struct$clustersRes$day) | 1)
              {
                if(length(struct$clustersRes$day) > 0 | 1)
                {
                  endClust <- cbind(struct$clustersRes, detailsRes)
                  endClust[, c("mcYear") := NULL]
                  detailResWrite <- try(sapply(unique(endClust$area),  function(ctry){
                    #for each country prepare file
                    endClustctry <- endClust[area == ctry]
                    orderBeg <- unique(endClustctry$time)
                    endClustctry[,c("area") := NULL]
                    
                    nameBy <- c("production", "col2")
                    
                    nomStruct <- names(endClustctry)[!names(endClustctry) %in% c("cluster", nameBy)]
                    
                    tmp_formula <- nomStruct
                    # tmp_formula <- gsub(" ", "_", tmp_formula)
                    tmp_formula <- paste0("`", tmp_formula, "`")
                    
                    tmp_formula <- as.formula(paste0(paste0(tmp_formula, collapse = " + "), "~cluster"))
                    
                    endClustctry[, c(nameBy) := list(round(`production`))]
                    
                    endClustctry <- data.table::dcast(endClustctry, tmp_formula,
                                                      value.var = c(nameBy))
                    
                    endClustctry <- endClustctry[match(orderBeg, endClustctry$time)]
                    endClustctry[,c("time", grep("col2",names(endClustctry),value = T)) := NULL]
                    nomStruct <- nomStruct[-which(nomStruct == "time")]
                    nomcair <- names(endClustctry)
                    nomcair <- nomcair[!nomcair%in%nomStruct]
                    nbvar <- length(nomcair)
                    unit <- rep("", length(nomcair))
                    unit[grep("production",nomcair)] <- "MWh"
                    nomcair <- gsub("production_","",nomcair)
                    Stats <- rep("EXP", length(unit))
                    nameIndex <- ifelse(type == "weekly", "week", "index")
                    nomStruct[which(nomStruct == "timeId")] <- nameIndex
                    indexMin <- min(endClustctry$timeId)
                    indexMax <- max(endClustctry$timeId)
                    ncolFix <- length(nomStruct)
                    #write detailsRes txt
                    .writeFileOut(dta = endClustctry, timestep = type, fileType = "details-res",
                                  ctry = ctry, opts = opts, folderType = "areas", nbvar = nbvar,
                                  indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                  nomcair = nomcair, unit = unit, nomStruct = nomStruct,Stats = Stats)
                  }), silent = TRUE)
                  .errorTest(detailResWrite, verbose, "Detail Res write")
                }
              }
            }
          }
          LOLD_data[, isLOLD_cum := 100 * isLOLD_cum/sum(mcWeights)]
        } 
      })
      
      # browser()
      .addMessage(verbose, paste0("------- End Mc-all : ", type, " -------"))
      outputlist <- .formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct) 
      list(outputlist, LOLD_data)
    }, verbose = verbose, simplify = FALSE)
    
    
    if(verbose>0) try({ close(pb) })
    if (writeOutput){
      if (tmstp == "hourly" | (tmstp == "annual" & !("hourly" %in% timestep))){
        # Create grid folder####
        .gridFolderCreation(opts, verbose)
        # Create digest####
        suppressWarnings(.writeDigestFile(opts, output[[1]], tmstp, linkTable, verbose, LOLD_data = output[[1]][[2]]))
      }
      
      mc_all <- file.path(opts$simDataPath, "mc-all")
      file.rename(from = mc_all, stri_replace_last_fixed(mc_all, "mc-all", paste0("mc-all-",tmstp)))
    }
    
    resultat[[tmstp]] <- output[[1]]
  }
  
  if (writeOutput){
    #aggregate timesteps####
    mc_all_hourly <- file.path(opts$simDataPath, "mc-all-hourly")
    mc_all <- file.path(opts$simDataPath, "mc-all")
    if (file.exists(mc_all_hourly)){
      file.rename(from = mc_all_hourly, str_replace(mc_all_hourly, "mc-all-hourly", "mc-all"))
    } else {
      dir.create(mc_all)
    }
    
    mc_alls <- grep("mc-all-", list.dirs(opts$simDataPath, recursive = F), value = T)
    for (mc_all_step in mc_alls){
      t <- Sys.time()
      files <- list.dirs(mc_all_step, recursive = F)
      file.copy(files, mc_all, recursive = T)
      if (verbose > 0){
        print(paste(mc_all_step,": Done"))
        print(Sys.time() - t)
      }
    }
    
    unlink(mc_alls, recursive = T)
    
    #add other nodes from original mc-all####
    original_mc_all_path <- file.path(opts$simDataPath,"original-mc-all")
    if (file.exists(original_mc_all_path)){
      print("Adding original mc-all data")
      old_areas <- list.dirs(file.path(original_mc_all_path,"areas"), recursive = F)
      old_links <- list.dirs(file.path(original_mc_all_path,"links"), recursive = F)
      old_thermal <- file.path(original_mc_all_path,"grid","thermal.txt")
      new_areas <- file.path(mc_all,"areas")
      new_links <- file.path(mc_all,"links")
      new_thermal <- file.path(mc_all,"grid")
      file.copy(old_areas, new_areas, overwrite = F, recursive = T)
      file.copy(old_links, new_links, overwrite = F, recursive = T)
      file.copy(old_thermal, new_thermal)
      
      
      ##merge digests 
      finalDigest <- mergeDigests(readDigestFile(opts),
                                  readDigestFile(opts, endpoint = "original-mc-all/grid/digest.txt"))
      suppressWarnings(writeDigest(finalDigest, opts))
    }
  }
  
  if(verbose > 0){
    print("mc-all total time :")
    print(Sys.time() - total_time)
  }
  
  if (!writeOutput){
    if (length(resultat) == 1) return (resultat[[1]])
    resultat
  }
}

#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param timestep \code{character} antares timestep
#' @param writeOutput \code{boolean} write result or not.
#' @param mcWeights \code{numeric} vector of weigth for mcYears.
#' @param mcYears  \code{numeric} mcYears to load.
#' @param filtering \code{boolean} filtering control
#' @param selected \code{list} named list (pass to antaresRead) : list(areas = 'a', links = 'a - e')
#' @param legacy \code{boolean} run old version of the function
#'
#' @export
#' 
#' @rdname aggregatate_mc_all
#' 
aggregateResult <- function(opts, 
                            verbose = 2, 
                            timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
                            writeOutput = TRUE, # for ADPatch compatibility
                            mcWeights = NULL,
                            mcYears = NULL,
                            filtering = FALSE,
                            selected = NULL,
                            legacy = FALSE){
  
  if (isFALSE(legacy)){
    parAggregateMCall(opts = opts, nbcl = 1, verbose = verbose, timestep = timestep, writeOutput = writeOutput, 
                      mcWeights = mcWeights, mcYears = mcYears, filtering = filtering,
                      selected = selected, legacy = legacy)
  } else {
    aggregateResult_old(opts = opts, verbose = verbose, filtering = filtering, 
                        selected = selected, timestep = timestep, writeOutput = writeOutput, 
                        mcWeights = mcWeights, mcYears = mcYears)
  }
  
  
}


#' @title Transform link table
#'
#' @description Transform link table to adapt to v8 antares
#'
#' @param linkTable \code{data.table} of data load which antaresRead::readAntares
#' @param RES_mode \code{boolean} True if clusters for renewables
#'
#' @return linkTableMini {data.table} filtered linkTable and adjusted to antares version
#'
#' @noRd
.transformLinkTable <- function(linkTable, RES_mode){
  linkTableMini <- linkTable[Folder == "area" & Mode == "economy" & Stats %in% c("EXP","values"),
                             c("Name","Unit","Stats")]
  
  ## linkTable doit etre adaptée pour la V8 dans le package
  ## adaptation manuelle pour le moment
  if (RES_mode == T){
    linkTableMini <- linkTableMini[!(Name %in% c("WIND", "SOLAR"))]
    linkTableMini <- rbind(linkTableMini[1:17],
                           setDT(as.list(c("MISC. DTG 2","MWh","EXP"))),
                           setDT(as.list(c("MISC. DTG 3","MWh","EXP"))),
                           setDT(as.list(c("MISC. DTG 4","MWh","EXP"))),
                           setDT(as.list(c("WIND OFFSHORE","MWh","EXP"))),
                           setDT(as.list(c("WIND ONSHORE","MWh","EXP"))),
                           setDT(as.list(c("SOLAR CONCRT.","MWh","EXP"))),
                           setDT(as.list(c("SOLAR PV","MWh","EXP"))),
                           setDT(as.list(c("SOLAR ROOFT","MWh","EXP"))),
                           setDT(as.list(c("RENW. 1","MWh","EXP"))),
                           setDT(as.list(c("RENW. 2","MWh","EXP"))),
                           setDT(as.list(c("RENW. 3","MWh","EXP"))),
                           setDT(as.list(c("RENW. 4","MWh","EXP"))),
                           linkTableMini[18:nrow(linkTableMini)],
                           use.names = F)
  }
  
  linkTableMini <- transpose(rbind(setDT(as.list(c("area","",""))),
                                   linkTableMini,
                                   use.names = F),
                             make.names = "V1")
  
  linkTableMini
}



#' @title Create links digest linear flow
#'
#' @description Create links digest linear flow
#'
#' @param testPar \code{list} of all data areas/links/clusters
#' @param tmstp \code{character} current timestep
#'
#' @return result {data.table} results for linear flow
#'
#' @noRd
.createDigestLinksLIN <- function(testPar, tmstp){
  digest <- testPar[["links"]][, c("link","FLOW LIN.")]
  ars <- unique(as.character(testPar[["areas"]][, area]))
  result <- data.table(matrix(0, nrow = length(ars), ncol = length(ars) + 1))
  setnames(result, c("...To",ars))
  result[, "...To" := ars]
  
  ## Aggregate by link
  if (tmstp != "annual"){
    digest <- digest[, .("FLOW LIN." = sum(`FLOW LIN.`)), by = link]
  }
  
  for (i in 1:nrow(digest)){
    data_link = strsplit(as.character(digest[i,link]), " - ")
    result[`...To` == data_link[[1]][2], data_link[[1]][1] := ifelse(round(as.numeric(digest[i,"FLOW LIN."])) == 0,
                                                                     1000000000000000,
                                                                     round(as.numeric(digest[i,"FLOW LIN."])))]
    result[`...To` == data_link[[1]][1], data_link[[1]][2] := ifelse(-round(as.numeric(digest[i,"FLOW LIN."])) == 0,
                                                                     1000000000000000,
                                                                     -round(as.numeric(digest[i,"FLOW LIN."])))]
    suppressWarnings(result[`...To` == data_link[[1]][1], data_link[[1]][1] := "X"])
  }
  result[result == 0] <- "--"
  result[result == 1000000000000000] <- 0
  for (col in colnames(result)[-1]) result <- result[`...To` == col, col := "X", with = F]
  
  result
}



#' @title Create links digest quadratic flow
#'
#' @description Create links digest quadratic flow
#'
#' @param testPar \code{list} of all data areas/links/clusters
#' @param tmstp \code{character} current timestep
#'
#' @return result {data.table} results for quadratic flow
#'
#' @noRd
.createDigestLinksQUAD <- function(testPar, tmstp){
  digest <- testPar[["links"]][, c("link","FLOW QUAD.")]
  ars <- unique(as.character(testPar[["areas"]][, area]))
  result <- data.table(matrix(0, nrow = length(ars), ncol = length(ars) + 1))
  setnames(result, c("...To",ars))
  result[, "...To" := ars]
  
  ## Aggregate by link
  if (tmstp != "annual"){
    digest <- digest[, .("FLOW QUAD." = sum(`FLOW QUAD.`)), by = link]
  }
  
  for (i in 1:nrow(digest)){
    data_link = strsplit(as.character(digest[i,link]), " - ")
    result[`...To` == data_link[[1]][2], data_link[[1]][1] := ifelse(round(as.numeric(digest[i,"FLOW QUAD."])) == 0,
                                                                     1000000000000000,
                                                                     round(as.numeric(digest[i,"FLOW QUAD."])))]
    result[`...To` == data_link[[1]][1], data_link[[1]][2] := ifelse(-round(as.numeric(digest[i,"FLOW QUAD."])) == 0,
                                                                     1000000000000000,
                                                                     -round(as.numeric(digest[i,"FLOW QUAD."])))]
    suppressWarnings(result[`...To` == data_link[[1]][1], data_link[[1]][1] := "X"])
  }
  result[result == 0] <- "--"
  result[result == 1000000000000000] <- 0
  for (col in colnames(result)[-1]) result <- result[`...To` == col, col := "X", with = F]
  
  result
}



#' @title Create areas digest from annual data
#'
#' @description Create areas digest from annual data
#'
#' @param testPar \code{list} of all data areas/links/clusters
#' @param linkTable \code{data.table} table of expressions
#'
#' @return digest {data.table} results for areas
#'
#' @noRd
.createDigestAreasAnnual <- function(testPar, linkTable, verbose){
  digest <- testPar[["areas"]]
  cols_remove = c("timeId", "time", grep("_",names(digest),value = T))
  digest <- digest[, -cols_remove, with = F]
  
  for(i in 2:length(names(digest)))
  {
    var <- names(digest)[i]
    dig <- linkTable[Folder == "area" & Mode == "economy" & Stats %in% c("EXP","values") & 
                       (Name == var | paste0(Name,"_",progNam) == var)]$digits
    if(length(dig)>0) digest[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
  }
  
  digest_names <- transpose(linkTable[Folder == "area" & Mode == "economy" & progNam == "EXP", c("Name", "Unit", "Stats")])
  setnames(digest_names, as.character(digest_names[1]))
  digest <- rbind(cbind(data.table(area = list("","")), digest_names[-1]), digest)
  if (verbose > 0) cat("Digest : Ok\n")
  digest
}



#' @title Create areas digest from hourly data
#'
#' @description Create areas digest from hourly data
#'
#' @param testPar \code{list} of all data areas/links/clusters
#' @param linkTable \code{data.table} table of expressions
#'
#' @return digest {data.table} results for areas
#'
#' @noRd
.createDigestAreasHourly <- function(testPar, linkTable, verbose, LOLD_data){
  digest <- testPar[["areas"]]
  cols_remove = c("month","hour","timeId","time","day", grep("_",names(digest),value = T))
  digest <- digest[, -cols_remove, with = F]
  
  colorder <- colnames(digest)
  digest <- digest[, `:=` (`MRG. PRICE` = `MRG. PRICE`/8736,
                           `H. LEV` = `H. LEV`/8736)][, lapply(.SD, sum, na.rm=TRUE), by="area"]
  digest <- merge(digest, LOLD_data)[, LOLP := isLOLD_cum][, isLOLD_cum := NULL]
  
  for(i in 2:length(names(digest)))
  {
    var <- names(digest)[i]
    dig <- linkTable[Folder == "area" & Mode == "economy" & Stats %in% c("EXP","values") & 
                       (Name == var | paste0(Name,"_",progNam) == var)]$digits
    if(length(dig)>0) digest[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
  }
  
  #rapid fix for extra columns in antares 8.1 (to do)
  digest_names <- transpose(linkTable[Folder == "area" & Mode == "economy" & progNam == "EXP", c("Name", "Unit", "Stats")])
  setnames(digest_names, as.character(digest_names[1]))
  digest <- rbind(cbind(data.table(area = list("","")), digest_names[-1]), digest)
  if (verbose > 0) cat("Digest (hourly) : Ok\n")
  
  digest
}



#' @title Create grid folder
#'
#' @description Create grid folder
#'
#' @noRd
.gridFolderCreation <- function(opts, verbose){
  ##create grid folder if doesnt exist
  grid_folder <- file.path(opts$simDataPath, "mc-all","grid")
  if (!dir.exists(grid_folder)){
    dir.create(paste0(opts$simDataPath, "/mc-all/grid"),
               recursive = TRUE, showWarnings = FALSE)
    ##areas.txt
    ars <- fread(file = paste0(opts$inputPath,"/areas/list.txt"), header = F)
    setnames(ars, "V1", "name")
    ars <- ars[, id := tolower(name)][, c("id","name")][order(id)]
    write.table(ars, file = paste0(opts$simDataPath, "/mc-all/grid/areas.txt"), 
                row.names = F, quote = F, sep = "\t")
    ##links.txt
    lnks <- getLinks(namesOnly = F)
    lnks <- sapply(lnks, function(X){X <- strsplit(X, " - ")})
    dt_lnks <- data.table()
    for (lnk in lnks){
      dt_lnks <- rbind(dt_lnks, data.table(upstream = lnk[1], downstream = lnk[2]))
    }
    dt_lnks <- dt_lnks[order(upstream)][!(is.na(upstream) | is.na(downstream))]
    write.table(dt_lnks, file = paste0(opts$simDataPath, "/mc-all/grid/links.txt"), 
                row.names = F, quote = F, sep = "\t")
    ##thermal.txt
    if (verbose > 0) cat("Grid folder : Ok\n")
  } else {if (verbose > 0) cat("Grid folder : already exists\n")}
}




#' @title Write digest file
#'
#' @description Write digest file
#'
#' @param output \code{list} of data after aggregation
#' @param tmstp \code{character} current timestep
#' @param linkTable \code{data.table} table of expressions
#' @param verbose \code{integer} level of console output detail
#' @param LOLD_data \code{data.table} data for number of mcYears with LOLD
#'
#' @noRd
.writeDigestFile <- function(opts, output, tmstp, linkTable, verbose, LOLD_data){
  digest <- list()

  if (tmstp == "annual") {
    digesta <- .createDigestAreasAnnual(output[[1]], linkTable, verbose)
  } else if (tmstp == "hourly") { digesta <- .createDigestAreasHourly(output[[1]], linkTable, verbose, LOLD_data)}
  
  digest$begin <- data.table(VARIABLES = ncol(digesta) - 1, AREAS = nrow(digesta) - 2, LINKS = 0)
  digest$areas <- digesta
  digest$middle <- data.table(VARIABLES = 0, AREAS = 0, LINKS = 0)
  digest$lin <- .createDigestLinksLIN(output[[1]], tmstp)
  digest$quad <- .createDigestLinksQUAD(output[[1]], tmstp)

  writeDigest(digest, opts)
}





#' @title parallel read antares
#'
#' @description read antares adapted for parallel execution
#'
#' @param mcYear \code{numeric} mcYear to read data
#' @param pth \code{character} study path
#' @param type \code{character} current timestep
#' @param areasselect \code{character} Areas to select 
#' @param linksSelect \code{character} links to select
#' @param clustersSelect \code{character} clusters to select
#' @param clustersResSelect \code{character} renewable clusters to select
#'
#' @noRd
.parReadAntares <- function(mcYear, pth, type, 
                            areasselect, linksSelect, 
                            clustersSelect, clustersResSelect){
  setSimulationPath(pth)
  dt <- readAntares(areas = areasselect, links = linksSelect, 
                    clusters = clustersSelect, clustersRes = clustersResSelect, 
                    timeStep = type, simplify = FALSE, 
                    mcYears = mcYear, showProgress = FALSE)
  dt
}



.createRecordYears <- function(value, year){
  recordYears <- list()
  
  recordYears$areas$min <- copy(value$areas$min)
  recordYears$areas$max <- copy(value$areas$max)
  recordYears$links$min <- copy(value$links$min)
  recordYears$links$max <- copy(value$links$max)
  
  recordYears$areas$min[!is.na(recordYears$areas$min)] <- year
  recordYears$areas$max[!is.na(recordYears$areas$max)] <- year
  recordYears$links$min[!is.na(recordYears$links$min)] <- year
  recordYears$links$max[!is.na(recordYears$links$max)] <- year
  
  recordYears
}

.updateRecordYears <- function(recordYears, value, valueTP, year){
  
  recordYears$areas$min[!is.na(recordYears$areas$min) & valueTP$areas$min < value$areas$min] <- year
  recordYears$areas$max[!is.na(recordYears$areas$max) & valueTP$areas$max > value$areas$max] <- year
  recordYears$links$min[!is.na(recordYears$links$min) & valueTP$links$min < value$links$min] <- year
  recordYears$links$max[!is.na(recordYears$links$max) & valueTP$links$max > value$links$max] <- year
  
  recordYears 
}


.formatOutput <- function(out, struct){
  
  # out <- lapply(value, function(X)(Reduce(cbind, X)))
  for(i in names(struct)){
    if(is.null(nrow(struct[[i]]))){
      struct[[i]] <- NULL
    } else {
      
      out[[i]]$TPmerge <- 1:nrow(out[[i]])
      struct[[i]]$TPmerge <- 1:nrow(struct[[i]])
      struct[[i]] <- merge(struct[[i]], out[[i]], by = 'TPmerge')
      struct[[i]]$TPmerge <- NULL
      suppressWarnings(
        struct[[i]][, c("mcYear", "OV. COST_min","CO2 EMIS._min", "ROW BAL._min", "PSP_min","MISC. NDG_min", "LOLP_min", "OV. COST_max", "CO2 EMIS._max",
                        "ROW BAL._max", "PSP_max", "MISC. NDG_max", "LOLP_max","OV. COST_std", "CO2 EMIS._std", "ROW BAL._std", "PSP_std", "MISC. NDG_std", "LOLP_std",
                        "LOOP FLOW_min", "FLOW QUAD._min", "CONG. PROB +_min", "CONG. PROB -_min", "LOOP FLOW_max", "FLOW QUAD._max", "CONG. PROB +_max", "CONG. PROB -_max", 
                        "LOOP FLOW_std", "FLOW QUAD._std", "CONG. PROB +_std", "CONG. PROB -_std" ) := NULL]
      )
      if(i == "clusters"){
        suppressWarnings(
          struct[[i]][, c( "production_min", "NP Cost_min", "NODU_min", "production_max", "NP Cost_max", "NODU_max", "production_std", "NP Cost_std", "NODU_std") := NULL]
        )
      }
      
      if(i == "clustersRes"){
        suppressWarnings(
          struct[[i]][, c( "production_min", "production_max", "production_std") := NULL]
        )
      }
      
      if(!is.null( struct[[i]]$day)){
        struct[[i]]$day <- as.numeric(struct[[i]]$day)
      }
    }
  }
  struct
}



#' @title Extract value part of data
#'
#' @description Extract value part of data
#'
#' @param dta \code{data.table} of data load which antaresRead::readAntares
#' @param SDcolsStartareas \code{numeric} first column of data for areas
#' @param SDcolsStartClust \code{numeric} first column of data for details
#'
#' @return value {data.table} value selected
#'
#' @noRd
.giveValue <- function(dta, SDcolsStartareas, SDcolsStartClust)
{
  value = list()
  if(!is.null(dta$areas)){
    value$areas <-  dta$areas[,lapply(.SD, as.numeric), .SDcols = (SDcolsStartareas+1):ncol(dta$areas)]
  }
  
  if(!is.null(dta$links)){
    value$links <- dta$links[,lapply(.SD, as.numeric), .SDcols = (SDcolsStartareas+1):ncol(dta$links)]
  }
  if(!is.null(dta[["clusters"]])){
    value$clusters <- dta[["clusters"]][,lapply(.SD, as.numeric), .SDcols = (SDcolsStartClust+1):ncol(dta[["clusters"]])]
  }
  if(!is.null(dta[["clustersRes"]])){
    value$clustersRes <- dta[["clustersRes"]][,lapply(.SD, as.numeric), .SDcols = (SDcolsStartClust+1):ncol(dta[["clustersRes"]])]
  }
  value
}

#' @title Create stat file compute min, max sd and mean
#'
#' @description Create stat file compute min, max sd and mean
#'
#' @param X \code{data.table} data load which
#' antaresRead::readAntares and extract which .giveValue
#'
#' @return res {data.table} stats computed
#'
#' @noRd
#'
.creatStats <- function(X, W_sum, w_sum2, mean_m, S, pond = 1){
  # browser()
  
  W_sum = W_sum + pond
  w_sum2 = w_sum2 + pond * pond
  mean_m2 = mean_m + (pond / W_sum) * (X - mean_m)
  
  S = S + pond * (X - mean_m2) * (X - mean_m)
  
  
  XP = X * pond
  res <- list(sum = XP, min = X, max = X, sumC = XP*XP, S = S, W_sum = W_sum, w_sum2 = w_sum2, mean_m = mean_m2)
  
  names(res$sum) <- paste(names(X))
  names(res$min) <- paste0(names(X), "_min")
  names(res$max) <- paste0(names(X), "_max")
  names(res$sumC) <- paste0(names(X), "_std")
  
  res
}

#' @title Update data min, max sd and mean
#'
#' @description Update data min, max sd and mean
#'
#' @param X \code{data.table} data init which .creatStats
#' @param Y \code{data.table} data load which
#' antaresRead::readAntares and extract which .giveValue
#'
#' @return X {data.table} stats updated
#'
#' @noRd
.updateStats <- function(X, Y){
  X$sum <-  X$sum + Y$sum
  X$min <-  pmin.fast(X$min , Y$min)
  X$max <-  pmax.fast(X$max , Y$max)
  X$sumC <-  X$sumC + Y$sumC
  X$var <- Y$S
  X$S <- Y$S
  X$W_sum <- Y$W_sum
  X$w_sum2 <- Y$w_sum2
  X$mean_m <- Y$mean_m
  
  X
}

# fast pmin and pmax for two elements
pmin.fast <- function(k,x) (x+k - abs(x-k))/2
pmax.fast <- function(k,x) (x+k + abs(x-k))/2



#' @title Write mc-all files
#'
#' @description Write mc-all files
#'
#' @param dta \code{data.table} data
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param fileType \code{character} must be values or details
#' @param ctry \code{character} country.
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param folderType \code{character} must be areas or links
#' @param nbvar \code{numeric} for header write, currently ncol(dta)
#' @param indexMin \code{numeric} for header write, depend of number of row write and calandar
#' @param indexMax \code{numeric} for header write, depend of number of row write and calandar
#' @param ncolFix \code{numeric} for header write
#' @param nomcair \code{character} for header write, names of variables
#' @param unit \code{character} for header write, unit of variables
#' @param nomStruct \code{character} for header write, depend of timestep
#' @param Stats \code{character} for header write, stats compute for eatch variables
#'
#'
#' @noRd
#'
.writeFileOut <- function(dta, timestep, fileType, ctry, opts, folderType, nbvar,
                          indexMin, indexMax, ncolFix, nomcair, unit, nomStruct, Stats){
  
  # threads for fwrite
  data.table::setDTthreads(2)
  
  folderTypesansS <- substr(folderType, 1, nchar(folderType)-1)
  abrtype <- substr(fileType, 1, 2)
  if (fileType == "details-res") abrtype <- "res"
  
  if(timestep == "annual"){
    nomStruct <- ""
    dta$timeId <- "Annual"
  }
  
  if(folderType == "links"){
    ctryDecomp <- strsplit(as.character(ctry), " - ")
    ctryDecomp <-  unlist(ctryDecomp)
    entete <- paste0(ctryDecomp[1], "\t",folderTypesansS,"\t",abrtype,
                     "\t",timestep,"\n",ctryDecomp[2] ,"\tVARIABLES\tBEGIN\tEND\n\t",
                     nbvar, "\t",indexMin, "\t",indexMax, "\n\n",
                     ctryDecomp[1], "\t", timestep, paste0(rep("\t", ncolFix), collapse = ""),
                     paste0(nomcair, collapse = "\t"),"\n",
                     paste0(rep("\t", ncolFix+1), collapse = ""),paste0(unit, collapse = "\t"),"\n",
                     "\t", paste0(nomStruct, collapse = "\t"), "\t", paste0(Stats, collapse = "\t"), "\n")
  }else{
    entete <- paste0(ctry, "\t",folderTypesansS,"\t",abrtype, "\t",timestep,"\n\tVARIABLES\tBEGIN\tEND\n\t",
                     nbvar, "\t",indexMin, "\t",indexMax, "\n\n",
                     ctry, "\t", timestep, paste0(rep("\t", ncolFix), collapse = ""),
                     paste0(nomcair, collapse = "\t"),"\n",
                     paste0(rep("\t", ncolFix+1), collapse = ""),paste0(unit, collapse = "\t"),"\n",
                     "\t", paste0(nomStruct, collapse = "\t"), "\t", paste0(Stats, collapse = "\t"), "\n")
  }
  
  dir.create(paste0(opts$simDataPath, "/mc-all", "/",folderType,"/", ctry),
             recursive = TRUE, showWarnings = FALSE)
  
  outputFile <- paste0(opts$simDataPath, "/mc-all", "/",folderType,"/", ctry, "/",
                       fileType, "-",timestep,".txt")
  
  file <- file(outputFile, "wb")
  
  write.table(entete,file , row.names = FALSE, eol = "",
              quote = FALSE, col.names = FALSE)
  
  # write.table(cbind(NA, dta), file,
  #             append = TRUE,
  #             row.names = FALSE,
  #             col.names =FALSE,
  #             quote = FALSE,sep = "\t",
  #             na = "")
  
  close(file)
  # cols <- setdiff(which(sapply(dta, is.numeric)), c("MRG. PRICE", "LOLD", "LOLP", "H. LEV"))
  # dta[, (cols) := lapply(.SD, round), .SDcols = cols]
  # if ("MRG. PRICE" %in% colnames(dta)) dta[, `MRG. PRICE` := round(`MRG. PRICE`, 2)]
  # if ("LOLD" %in% colnames(dta)) dta[, `LOLD` := round(`LOLD`, 2)]
  # if ("LOLP" %in% colnames(dta)) dta[, `LOLP` := round(`LOLP`, 2)]
  # if ("H. LEV" %in% colnames(dta)) dta[, `H. LEV` := round(`H. LEV`, 2)]
  data.table::fwrite(cbind(NA, dta), outputFile,
                     append = TRUE,
                     row.names = FALSE,
                     quote = FALSE, sep = "\t",
                     eol = "\n", na = "N/A")
  
  
}


#' @title Edit info on output folder
#'
#' @description Edit info on output folder
#'
#' @param outData \code{character} out folder path
#' @param simulationName \code{character} simulation name
#' @param dateTim2 \code{datetime} simulation begin date time
#' @param dtTim \code{datetime} simulation end date time
#'
#' @noRd
.editOutputInfo <- function(outData, simulationName, dateTim2, dtTim)
{
  #Edit infos output simulation
  iniPath <- paste0(outData, "/info.antares-output")
  infosIni <- readIniFile(iniPath)
  infosIni$general$name <- substr(simulationName, 1, nchar(simulationName)-10)
  dateTim2 <- gsub("-" , ".", dateTim2)
  dateTim2 <- gsub(" " , " - ", dateTim2)
  infosIni$general$date <- dateTim2
  infosIni$general$title <- dateTim2
  infosIni$general$timestamp <- round(as.numeric(difftime(dtTim,
                                                          as.POSIXct("1970-01-01 00:00:00"), units = "sec")), 0)
  .writeIni(infosIni, iniPath)
}


#' @title Progress bar
#'
#' @description Progress bar
#'
#' @param pb \code{progressbar} progress bar to update
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param timestep \code{character} must be annual, monthly, weekly, daily or hourly
#' @param mcALLNum \code{numeric} current mcYears position
#' @param nbmcallTOT \code{numeric} number of  mcYear
#'
#' @return progress bar update
#'
#' @noRd
.progBar <- function(pb, timeStep, mcALLNum, nbmcallTOT, coef = 1, terminate = FALSE)
{
  
  usalTime <- data.frame(period = c("start","annual", "daily", "hourly", "monthly", "weekly"),
                         value = c(0, 20, 200, 600, 800, 1000))
  per <- which(usalTime$period == timeStep)
  dif <- usalTime$value[per] - usalTime$value[per - 1]
  approxEnd <- mcALLNum*coef/nbmcallTOT
  i = (dif * approxEnd + usalTime$value[per - 1]) / usalTime$value[length( usalTime$value)]
  if(terminate&!is.null(pb)){setTxtProgressBar(pb, 1)}else{ setTxtProgressBar(pb, i)}
  
  
  
}


.getAreasToAggregate <- function(opts, timeStep){
  inputsAreas <- paste0(opts$studyPath, "/input/areas")
  allAreas <- list.dirs(inputsAreas, full.names = FALSE)
  allAreas <- allAreas[!allAreas == ""]
  out <- sapply(allAreas, function(X){
    Ini <- paste0(inputsAreas, "/", X, "/optimization.ini")
    gsub(" ", "", unlist(strsplit(readIniFile(Ini)$filtering$`filter-synthesis`, ",")))
  }, simplify = FALSE)
  
  out <- lapply(out, function(X){
    any(X == timeStep)
  })
  names(out)[which(unlist(out))]
}



.getLinksToAggregate <- function(opts, timeStep){
  inputsLinks <- paste0(opts$studyPath, "/input/links")
  allLinks <- list.dirs(inputsLinks, full.names = FALSE)
  allLinks <- allLinks[!allLinks == ""]
  out <- sapply(allLinks, function(X){
    Ini <- paste0(inputsLinks, "/", X, "/properties.ini")
    res <- lapply(readIniFile(Ini), function(X){X$`filter-synthesis`})
    lapply(res, function(X)gsub(" ", "", unlist(strsplit(X, ","))))
  })
  out <- out[lapply(out, length)!=0]
  out <- lapply(out, function(X){
    lapply(X, function(Y){
      any(Y == timeStep)
    })
  })
  out <- lapply(out, unlist)
  out <- lapply(out, function(X)X[X])
  res <- list()
  for(i in 1:length(out)){
    res[[i]] <- paste(names(out)[i], names(out[[i]]), sep = " - ")
  }
  res <- unlist(res)
  res
}


.recupeFilesUser <- function(opts){
  if(dir.exists(paste0(opts$studyPath, "/user/tempfile/")))
  {
    tocp <- paste0(opts$studyPath, "/user/tempfile/", list.files(paste0(opts$studyPath, "/user/tempfile"), recursive = TRUE))
    initialFile <- gsub("/user/tempfile/","/input/", tocp)
    file.copy(tocp, initialFile, overwrite = TRUE)
    unlink(paste0(opts$studyPath, "/user/tempfile"), recursive = TRUE)
  }
}



#' @title error test
#'
#' @description error test
#'
#' @param tryReturn return from try
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' @param msg \code{character} message to display if no error
#'
#' @noRd
.errorTest <- function(tryReturn, verbose, msg)
{
  if("try-error" %in% class(tryReturn)){
    stop(msg, " : ", tryReturn[1])
  }else{
    if(verbose == 2){
      cat(paste0(msg, " : Ok\n"))
    }
  }
}

#' @title Display message on console
#'
#' @description Display message on console
#'
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' @param valAf \code{numeric} show log in console if valAf == verbose.
#' @param msg \code{character} message to display if no error
#'
#' @noRd
.addMessage <- function(verbose, msg, valAf = 2){
  if(verbose == valAf){
    cat(paste0(msg, "\n"))
  }
}







# From antaresFlowbased duplicated necessary
#' Write ini file from list obtain by antaresRead::readIniFile and modify by user
#'
#' @param listData \code{list}, modified list obtained by antaresRead::readIniFile.
#' @param pathIni \code{Character}, Path to ini file.
#' @param overwrite logical, should file be overwritten if already exist?
#'
#' @examples
#'
#' \dontrun{
#' pathIni <- "D:/exemple_test/settings/generaldata.ini"
#' generalSetting <- antaresRead::readIniFile(pathIni)
#' generalSetting$output$synthesis <- FALSE
#' writeIni(generalSetting, pathIni)
#' }
#'
#' @keywords internal
#'
.writeIni <- function(listData, pathIni, overwrite = FALSE) {
  if (file.exists(pathIni)) {
    if (overwrite) {
      file.remove(pathIni)
    } else {
      stop("files already exist")
    }
  }
  con <- file(pathIni, "wb")
  on.exit(close(con))
  invisible(
    lapply(
      X = seq_along(listData),
      FUN = .formatedIniList,
      dtaToTransform = listData,
      namesdtaToTransform = names(listData),
      con = con
    )
  )
}

#' Change R format to ini format
#' @param val value to format
#'
#' @return val formated value
#'
#' @noRd
.formatedIni <- function(val) {
  if (class(val) %in% c("numeric", "integer")) {
    format(val, nsmall = 6, scientific = FALSE)
  } else if (class(val) %in% c("logical")) {
    if (is.na(val)) {
      ""
    } else {
      tolower(as.character(val))
    }
  } else {
    val
  }
}

#' write ini (raw by raw)
#'
#' @param dtaToTransform \code{list} data to write
#' @param namesdtaToTransform \code{character} names of data to write
#' @param con file connection where data are write
#'
#' @noRd
.formatedIniList <- function(x, dtaToTransform, namesdtaToTransform, con) {
  if (length(dtaToTransform[[x]]) > 0) {
    if (!is.null(namesdtaToTransform)) {
      writeChar( paste0("[", namesdtaToTransform[x], "]\n"), con, eos = NULL)
    } else {
      writeChar(paste0("[", x-1, "]\n"), con, eos = NULL)
    }
    tmp_data <- dtaToTransform[[x]]
    # format values
    values <- lapply(X = tmp_data, FUN = .formatedIni)
    values <- lapply(X = values, FUN = paste, collapse = ", ")
    # write
    writeChar(paste(paste0(names(tmp_data), " = ", values), collapse = "\n"), con, eos = NULL)
    writeChar("\n\n", con, eos = NULL)
  } else {
    if (nzchar(namesdtaToTransform[x]))
      writeChar( paste0("[", namesdtaToTransform[x], "]\n"), con, eos = NULL)
    writeChar("\n\n", con, eos = NULL)
  }
}




#' Creation of Mc_all (only antares > V6)
#'
#' Creation of Mc_all (only antares > V6)
#' 
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}
#' @param verbose \code{numeric} show log in console. Defaut to 1
#' \itemize{
#'  \item{0}{ : No log}
#'  \item{1}{ : Short log}
#'  \item{2}{ : Long log}
#'}
#' @param filtering \code{boolean} filtering control
#' @param timestep \code{character} antares timestep
#' @param selected \code{list} named list (pass to antaresRead) : list(areas = 'a', links = 'a - e')
#' @param writeOutput \code{boolean} write result or not.
#' @param mcWeights \code{numeric} vector of weigth for mcYears.
#' @param mcYears  \code{numeric} mcYears to load.
#'
#' @import data.table
#' 
#' @noRd
#' 
#' @examples
#' \dontrun{
#'    aggregateResult(opts)
#'
#' }
#' 
aggregateResult_old <- function(opts, verbose = 1,
                                filtering = FALSE,
                                selected = NULL,
                                timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
                                writeOutput = FALSE,
                                mcWeights = NULL,
                                mcYears = NULL){
  
  
  # browser()
  
  Folder <- Files <- Mode <- Name <- progNam <- `production EXP` <- `NODU EXP`<- `NP Cost EXP` <- `production` <- `NODU`<- `NP Cost` <- NULL
  # opts
  # verbose = 1
  # filtering = FALSE
  # selected = NULL
  # timestep = "houry"
  # writeOutput = FALSE
  # mcWeights = c(1, 2)
  # mcYears = c(1, 3)
  # 
  if(writeOutput == FALSE){
    coef = 1.4
  }else{
    coef = 1
  }
  
  if(writeOutput == FALSE & length(timestep)>1){
    stop("If you want data return you must choose a unique timestep")
  }
  
  
  if(verbose > 0)
  {
    try({
      pb <- txtProgressBar(style = 3)
    })
  }
  
  suppressWarnings(
    opts <- setSimulationPath(opts$simPath)
  )
  
  # Version which readAntares
  linkTable <- try({
    data.table::fread(system.file("/format_output/tableOutput_aggreg.csv", package = "antaresRead"))},
    silent = TRUE
  )
  .errorTest(linkTable, verbose, "\nLoad of link table")
  
  # load link table
  linkTable$progNam <- linkTable$Stats
  linkTable$progNam[which(linkTable$progNam == "values")] <- "EXP"
  # dtaMc <- paste0(opts$simDataPath, "/mc-ind")
  if(!is.null(mcYears)){
    numMc <- mcYears
    if(length(numMc) == 1){
      if(numMc%in% c("all", "All")){
        numMc <- opts$mcYears
      }
    }
  } else{
    numMc <- opts$mcYears
  }
  if(is.null(mcWeights)){
    mcWeights <- rep(1, length(numMc))
  }
  
  if(length(mcWeights)!=length(numMc)){
    stop('length of mcWeights must be the same as mcYears')
  }
  
  coef_div_mc_pond <- sum(mcWeights)
  coef_div_mc_pond_2 <- sum(mcWeights * mcWeights)
  
  #sapply on timeStep
  #allTyped <- c("annual", "daily", "hourly", "monthly", "weekly")
  #allTyped <- 'hourly'
  
  output <- sapply(timestep, function(type, verbose)
  {
    
    .addMessage(verbose, paste0("------- Mc-all : ", type, " -------"))
    
    suppressWarnings(
      try({
        
        # browser()
        # load first MC-year
        a <- Sys.time()
        
        if(!filtering)
        {
          dta <- readAntares(areas = "all", links = "all", clusters = "all",
                             clustersRes = "all", timeStep = type, simplify = FALSE, 
                             mcYears = numMc[1], showProgress = FALSE)
        } else {
          if(is.null(selected)){
            
            areasselect <- .getAreasToAggregate(opts, type)
            linksSelect <- .getLinksToAggregate(opts, type)
            dta <- readAntares(areas = areasselect,
                               links = linksSelect,
                               clusters = areasselect,
                               clustersRes = areasselect,
                               timeStep = type,
                               simplify = FALSE,
                               mcYears = numMc[1],
                               showProgress = FALSE)
          } else {
            dta <- readAntares(areas = selected$areas,
                               links = selected$links,
                               clusters = selected[["clusters"]],
                               clustersRes = selected[["clustersRes"]],
                               timeStep = type,
                               simplify = FALSE,
                               mcYears = numMc[1],
                               showProgress = FALSE)
            
          }
        }
        
        if(length(dta)>0){
          
          dtaLoadAndcalcul <- try({
            
            aTot <- as.numeric(Sys.time() - a)
            
            SDcolsStartareas <- switch(type,
                                       daily = 6,
                                       annual = 4,
                                       hourly = 7,
                                       monthly = 5,
                                       weekly = 4
            )
            
            SDcolsStartClust <-  SDcolsStartareas + 1
            #make structure
            
            struct <- list()
            if(!is.null(dta[["areas"]])){
              struct$areas <- dta$areas[,.SD, .SDcols = 1:SDcolsStartareas]
            }
            
            if(!is.null(dta[["links"]])){
              struct$links <- dta$links[,.SD, .SDcols = 1:SDcolsStartareas]
            }
            
            if(!is.null(dta[["clusters"]])){
              struct$clusters <- dta[["clusters"]][,.SD, .SDcols = 1:SDcolsStartClust]
            }
            
            if(!is.null(dta[["clustersRes"]])){
              struct$clustersRes <- dta[["clustersRes"]][,.SD, .SDcols = 1:SDcolsStartClust]
            }
            
            if(type == "weekly"){
              if(!is.null(struct[["areas"]])){
                struct$areas$timeId <- as.numeric(substr(struct$areas$time, nchar(as.character(struct$areas$time[1]))-1,
                                                         nchar(as.character(struct$areas$time[1]))))
              }
              
              if(!is.null(struct[["links"]])){
                struct$links$timeId <- as.numeric(substr(struct$link$time, nchar(as.character(struct$link$time[1]))-1,
                                                         nchar(as.character(struct$link$time[1]))))
              }
              
              if(!is.null(struct[["clusters"]])){
                struct$clusters$timeId <- as.numeric(substr(struct$clusters$time, nchar(as.character(struct$clusters$time[1]))-1,
                                                            nchar(as.character(struct$clusters$time[1]))))
              }
              
              if(!is.null(struct[["clustersRes"]])){
                struct$clustersRes$timeId <- as.numeric(substr(struct$clustersRes$time, nchar(as.character(struct$clustersRes$time[1]))-1,
                                                               nchar(as.character(struct$clustersRes$time[1]))))
              }
            }
            
            if(!is.null(struct$areas$day)){
              struct$areas$day <- ifelse(nchar(struct$areas$day) == 1,
                                         paste0("0", struct$areas$day),
                                         as.character(struct$areas$day))
            }
            if(!is.null(struct$links$day)){
              struct$links$day <- ifelse(nchar(struct$links$day) == 1,
                                         paste0("0", struct$links$day),
                                         as.character(struct$links$day))
            }
            if(!is.null(struct[["clusters"]]$day)){
              struct$clusters$day <- ifelse(nchar(struct$clusters$day) == 1,
                                            paste0("0", struct$clusters$day),
                                            as.character(struct$clusters$day))
            }
            
            if(!is.null(struct[["clustersRes"]]$day)){
              struct$clustersRes$day <- ifelse(nchar(struct$clustersRes$day) == 1,
                                               paste0("0", struct$clustersRes$day),
                                               as.character(struct$clustersRes$day))
            }
            
            b <- Sys.time()
            #value structure
            value <- .giveValue(dta, SDcolsStartareas, SDcolsStartClust)
            N <- length(numMc)
            
            W_sum = 0
            w_sum2 = 0
            mean_m = 0
            S = 0
            
            value <- lapply(value, function(X){.creatStats(X, W_sum, w_sum2, mean_m, S, mcWeights[1])})
            
            btot <- as.numeric(Sys.time() - b)
            if(verbose>0)
            {
              try({
                .progBar(pb, type, 1, N, coef)
              })
            }
            #sequentially add values
            if(N>1)
            {
              for(i in 2:N){
                a <- Sys.time()
                
                if(!filtering)
                {
                  dtaTP <- readAntares(areas = "all", links = "all", clusters = "all", clustersRes = "all",
                                       timeStep = type, simplify = FALSE, mcYears = numMc[i], showProgress = FALSE)
                } else {
                  
                  if(is.null(selected)){
                    
                    dtaTP <- readAntares(areas = areasselect, 
                                         links = linksSelect, 
                                         clusters = areasselect, 
                                         clustersRes = areasselect,
                                         timeStep = type, simplify = FALSE, 
                                         mcYears = numMc[i], showProgress = FALSE)
                  } else{
                    dtaTP <- readAntares(areas = selected$areas,
                                         links = selected$links,
                                         clusters = selected[["clusters"]],
                                         clustersRes = selected[["clustersRes"]],
                                         timeStep = type,
                                         simplify = FALSE,
                                         mcYears = numMc[i],
                                         showProgress = FALSE)
                    
                  }
                }
                
                aTot <- aTot + as.numeric(Sys.time() - a)
                b <- Sys.time()
                
                valueTP <- .giveValue(dtaTP, SDcolsStartareas, SDcolsStartClust)
                
                nmKeep <- names(valueTP)
                
                valueTP <- lapply(names(valueTP), function(X){
                  
                  .creatStats(valueTP[[X]], value[[X]]$W_sum, value[[X]]$w_sum2, value[[X]]$mean_m, value[[X]]$S , mcWeights[i])
                  
                })
                
                names(valueTP) <- nmKeep 
                
                # valueTP <- mapply(function(X, Y){.creatStats(X, Y$W_sum, Y$w_sum2, Y$mean_m, Y$S , mcWeights[i])}, X = valueTP, Y = value, SIMPLIFY = FALSE)
                
                value$areas <- .updateStats(value[["areas"]], valueTP[["areas"]])
                value$links <- .updateStats(value[["links"]], valueTP[["links"]])
                value$clusters <- .updateStats(value[["clusters"]], valueTP[["clusters"]])
                value$clustersRes <- .updateStats(value[["clustersRes"]], valueTP[["clustersRes"]])
                
                btot <- btot + as.numeric(Sys.time() - b)
                if(verbose>0)
                {
                  try({
                    .progBar(pb, type, i, N, coef)
                  })
                }
              }
              
              
              #Calcul of sd
              b <- Sys.time()
              
              coef_div_var = (coef_div_mc_pond ) 
              value$areas$std <- sqrt(value$areas$var / coef_div_var)
              #nan due to round
              for (i in names(value$areas$std))
                value$areas$std[is.nan(get(i)), (i) := 0]
              
              value$links$std <- sqrt(value$links$var / coef_div_var)
              #nan due to round
              for (i in names(value$links$std))
                value$links$std[is.nan(get(i)), (i) := 0]
              
              if(!is.null(value[["clusters"]])){
                value$clusters$std <- sqrt(value$clusters$var / coef_div_var)
                #nan due to round
                for (i in names(value$clusters$std))
                  value$clusters$std[is.nan(get(i)), (i) := 0]
              }
              
              if(!is.null(value[["clustersRes"]])){
                value$clustersRes$std <- sqrt(value$clustersRes$var / coef_div_var)
                #nan due to round
                for (i in names(value$clustersRes$std))
                  value$clustersRes$std[is.nan(get(i)), (i) := 0]
              }
            } else {
              # std to 0
              value <- lapply(value, function(x){
                if(!is.null(x$sumC)){
                  x$std <- x$sumC
                  x$std[, c(colnames(x$std)) := lapply(.SD, function(x) 0), .SDcols = colnames(x$std)]
                  colnames(x$std) <- gsub("_std$", "", colnames(x$std))
                  x
                }
              })
            }
            
            
            value$areas$sumC <- NULL
            value$links$sumC <- NULL
            
            if(!is.null(value[["clusters"]])){
              value$clusters$sumC <- NULL
            }
            if(!is.null(value[["clustersRes"]])){
              value$clustersRes$sumC <- NULL
            }
            value$areas$var <- NULL
            value$areas$S <- NULL
            value$areas$W_sum <- NULL
            value$areas$w_sum2 <- NULL
            value$areas$mean_m <- NULL
            
            value$links$var <- NULL
            value$links$S <- NULL
            value$links$W_sum <- NULL
            value$links$w_sum2 <- NULL
            value$links$mean_m <- NULL
            
            if(!is.null(value[["clusters"]])){
              value$clusters$var <- NULL
              value$clusters$S <- NULL
              value$clusters$W_sum <- NULL
              value$clusters$w_sum2 <- NULL
              value$clusters$mean_m <- NULL
            }
            
            if(!is.null(value[["clustersRes"]])){
              value$clustersRes$var <- NULL
              value$clustersRes$S <- NULL
              value$clustersRes$W_sum <- NULL
              value$clustersRes$w_sum2 <- NULL
              value$clustersRes$mean_m <- NULL
            }
            
            if(!is.null(value$areas) && !is.null(names(value$areas$std))){names(value$areas$std) <- paste0(names(value$areas$std) , "_std")}
            if(!is.null(value$links) && !is.null(names(value$links$std))){names(value$links$std) <- paste0(names(value$links$std) , "_std")}
            if(!is.null(value[["clusters"]]) && !is.null(names(value[["clusters"]]$std))){names(value[["clusters"]]$std) <- paste0(names(value[["clusters"]]$std) , "_std")}
            if(!is.null(value[["clustersRes"]]) && !is.null(names(value[["clustersRes"]]$std))){names(value[["clustersRes"]]$std) <- paste0(names(value[["clustersRes"]]$std) , "_std")}
            
            value$areas$sum <- value$areas$sum / coef_div_mc_pond
            value$links$sum <- value$links$sum / coef_div_mc_pond
            if(!is.null(value[["clusters"]])){
              value$clusters$sum <- value$clusters$sum / coef_div_mc_pond
            }
            if(!is.null(value[["clustersRes"]])){
              value$clustersRes$sum <- value$clustersRes$sum / coef_div_mc_pond
            }
            
            
            btot <- btot + as.numeric(Sys.time() - b)
            .addMessage(verbose, paste0("Time for reading data : ", round(aTot,1), " secondes"))
            .addMessage(verbose, paste0("Time for calculating : ", round(btot,1), " secondes"))
          }, silent = TRUE)
          
          .errorTest(dtaLoadAndcalcul, verbose, "\nLoad data and calcul")
          
          # browser()
          
          #Write area
          allfiles <- c("values")
          
          if(writeOutput == FALSE){
            if(verbose>0)
            {
              .progBar(pb, type, 1, 1, 1, terminate = TRUE)
            }
            
            return(.formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct))
          } else {
            
            if(!is.null(value$clustersRes) && is.data.frame(value$clustersRes) && nrow(value$clustersRes) > 0){
              warning("Writing clusterRes file is not at moment available")
            }
            
            areaWrite <- try(sapply(allfiles, function(f)
            {
              #prepare data for all country
              areaSpecialFile <- linkTable[Folder == "area" & Files == f & Mode == tolower(opts$mode)]
              namekeep <- paste(areaSpecialFile$Name, areaSpecialFile$Stats)
              namekeepprog <- paste(areaSpecialFile$Name, areaSpecialFile$progNam)
              areas <- cbind(value$areas$sum,  value$areas$std, value$areas$min, value$areas$max)
              if(nrow(areas) > 0)
              {
                
                areas <- areas[, .SD, .SDcols = which(names(areas)%in%opts$variables$areas)]
                areas <- areas[, .SD, .SDcols = match(opts$variables$areas, names(areas))]
                
                nbvar <- ncol(areas)
                areas <- cbind(struct$areas, areas)
                ncolFix <- ncol(struct$areas) - 3
                areas[, c("mcYear", "time") := NULL]
                allAreas <- unique(areas$area)
                
                for(i in 1:length(opts$variables$areas))
                {
                  var <- opts$variables$areas[i]
                  dig <- areaSpecialFile[var == paste(Name,progNam )]$digits
                  if(length(dig)>0)areas[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
                }
                
                
                if(length(allAreas) > 0)
                {
                  sapply(allAreas,  function(areasel){
                    #for each country prepare file
                    areastowrite <- areas[area == areasel]
                    areastowrite[,c("area") := NULL]
                    indexMin <- min(areas$timeId)
                    indexMax <- max(areas$timeId)
                    kepNam <- names(struct$areas)[!names(struct$areas)%in%c("area","mcYear","time")]
                    nameIndex <- ifelse(type == "weekly", "week", "index")
                    kepNam[which(kepNam == "timeId")] <- nameIndex
                    #write txt
                    .writeFileOut(dta = areastowrite, timestep = type, fileType = f,
                                  ctry = areasel, opts = opts, folderType = "areas", nbvar = nbvar,
                                  indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                  nomcair = areaSpecialFile$Name, unit = areaSpecialFile$Unit,
                                  nomStruct = kepNam,Stats = areaSpecialFile$Stats)
                    
                    
                  })
                }
              }
            }), silent = TRUE)
            
            .errorTest(areaWrite, verbose, "Area write")
            
            allfiles <- c("values")
            linkWrite <- try(sapply(allfiles, function(f)
            {
              #prepare data for all link
              linkSpecialFile <- linkTable[Folder == "link" & Files == f & Mode == tolower(opts$mode)]
              namekeep <- paste(linkSpecialFile$Name, linkSpecialFile$Stats)
              namekeepprog <- paste(linkSpecialFile$Name, linkSpecialFile$progNam)
              links <- cbind(value$links$sum,  value$links$std, value$links$min, value$links$max)
              if(nrow(links) > 0)
              {
                
                
                links <- links[, .SD, .SDcols = which(names(links)%in%opts$variables$links)]
                links <- links[, .SD, .SDcols = match(opts$variables$links, names(links))]
                
                # 
                # areas <- areas[, .SD, .SDcols = which(names(areas)%in%opts$variables$links)]
                # areas <- areas[, .SD, .SDcols = match(opts$variables$areas, names(areas))]
                # 
                # 
                
                nbvar <- ncol(links)
                links <- cbind(struct$links, links)
                ncolFix <- ncol(struct$links)-3
                links[, c("mcYear", "time") := NULL]
                allLink<- unique(links$link)
                
                for(i in 1:length(opts$variables$links))
                {
                  var <- opts$variables$links[i]
                  dig <- linkSpecialFile[var == paste(Name,progNam )]$digits
                  if(length(dig)>0)links[, c(var) := .(do.call(round, args = list(get(var), digits = dig)))]
                }
                
                sapply(allLink,  function(linksel){
                  #for eatch link prepare file
                  linkstowrite <- links[link == linksel]
                  linkstowrite[,c("link") := NULL]
                  indexMin <- min(links$timeId)
                  indexMax <- max(links$timeId)
                  kepNam <- names(struct$link)[!names(struct$link)%in%c("link","mcYear","time")]
                  nameIndex <- ifelse(type == "weekly", "week", "index")
                  kepNam[which(kepNam == "timeId")] <- nameIndex
                  #write txt
                  .writeFileOut(dta = linkstowrite, timestep = type, fileType = f,
                                ctry = linksel, opts = opts, folderType = "links", nbvar = nbvar,
                                indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                nomcair = linkSpecialFile$Name, unit = linkSpecialFile$Unit,
                                nomStruct = kepNam,Stats = linkSpecialFile$Stats)
                })
              }
            }), silent = TRUE)
            
            .errorTest(linkWrite, verbose, "Link write")
            
            ##Details
            details <- value$clusters$sum
            
            if(!is.null(struct$clusters$day))
            {
              if(length(struct$clusters$day) > 0)
              {
                endClust <- cbind(struct$clusters, details)
                
                endClust[, c("mcYear") := NULL]
                
                detailWrite <- try(sapply(unique(endClust$area),  function(ctry){
                  #for each country prepare file
                  endClustctry <- endClust[area == ctry]
                  orderBeg <- unique(endClustctry$time)
                  endClustctry[,c("area") := NULL]
                  
                  if(tolower(opts$mode) == "economy")
                  {
                    nameBy <- c("production", "NP Cost", "NODU")
                  }else{
                    nameBy <- c("production")
                  }
                  # if("NP Cost"%in%names(endClustctry)){}
                  nomStruct <- names(endClustctry)[!names(endClustctry) %in% c("cluster", nameBy)]
                  
                  tmp_formula <- nomStruct
                  # tmp_formula <- gsub(" ", "_", tmp_formula)
                  tmp_formula <- paste0("`", tmp_formula, "`")
                  
                  tmp_formula <- as.formula(paste0(paste0(tmp_formula, collapse = " + "), "~cluster"))
                  
                  if(tolower(opts$mode) == "economy")
                  {
                    endClustctry[, c(nameBy) := list(round(`production`),
                                                     round(`NP Cost`),
                                                     round(`NODU`))]
                  }else{
                    endClustctry[, c(nameBy) := list(round(`production`))]
                  }
                  
                  endClustctry <- data.table::dcast(endClustctry, tmp_formula,
                                                    value.var = c(nameBy))
                  
                  endClustctry <- endClustctry[match(orderBeg, endClustctry$time)]
                  endClustctry[,c("time") := NULL]
                  nomStruct <- nomStruct[-which(nomStruct == "time")]
                  nomcair <- names(endClustctry)
                  nomcair <- nomcair[!nomcair%in%nomStruct]
                  nbvar <- length(nomcair)
                  unit <- rep("", length(nomcair))
                  unit[grep("production",nomcair)] <- "MWh"
                  unit[grep("NP Cost",nomcair)] <- "NP Cost - Euro"
                  unit[grep("NODU",nomcair)] <- "NODU"
                  nomcair <- gsub("production","",nomcair)
                  nomcair <- gsub("NP Cost","",nomcair)
                  nomcair <- gsub("NODU","",nomcair)
                  Stats <- rep("EXP", length(unit))
                  nameIndex <- ifelse(type == "weekly", "week", "index")
                  nomStruct[which(nomStruct == "timeId")] <- nameIndex
                  indexMin <- min(endClustctry$timeId)
                  indexMax <- max(endClustctry$timeId)
                  ncolFix <- length(nomStruct)
                  #write details txt
                  .writeFileOut(dta = endClustctry, timestep = type, fileType = "details",
                                ctry = ctry, opts = opts, folderType = "areas", nbvar = nbvar,
                                indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                nomcair = nomcair, unit = unit, nomStruct = nomStruct,Stats = Stats)
                }), silent = TRUE)
                .errorTest(detailWrite, verbose, "Detail write")
              }
            }
          }
        }
      })
    )
    
    .addMessage(verbose, paste0("------- End Mc-all : ", type, " -------"))
    .formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct)
  }, verbose = verbose, simplify = FALSE)
  
  
  if(verbose>0)
  {
    try({
      close(pb)
    })
  }
  if(length(output)==1)return(output[[1]])
  output
  
  
}



#' @param nbcl \code{numeric} Number of parralel process
#' @param verbose \code{numeric} see logs (1) or not (0)
#'
#' @import data.table parallel
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom pbapply pblapply pboptions
#' @importFrom doParallel registerDoParallel
#'
#' @noRd
#' 
parAggregateMCall_old <- function(opts,
                                  nbcl = 8,
                                  verbose = 1,
                                  timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
                                  writeOutput = TRUE, # for ADPatch compatibility
                                  mcWeights = NULL,
                                  mcYears = NULL){
  
  
  if(verbose == 1){
    cat("Mc all start\n")
  }
  
  if(verbose == 0){
    pboptions(type = "none")
  }
  
  areas <- getAreas(opts = opts)
  links <- getLinks(opts = opts)
  # clusters <- areas
  clusters <- getAreas(withClustersOnly = TRUE, opts = opts)
  
  todo <- data.table(V1 = c(areas, links, clusters),
                     V2 = c(rep("area", length(areas)),
                            rep("link", length(links)),
                            rep("cluster", length(clusters))
                     ))
  
  todo <- apply(todo, 1, function(X)list(X))
  
  if(nbcl>1){
    parallel <- TRUE
  }else{
    parallel <- FALSE
  }
  
  if(parallel){
    cl <- makeCluster(nbcl)
    clusterExport(cl, c("opts", "timestep", "writeOutput", "mcWeights", "mcYears"), envir = environment())
    registerDoParallel(cl)
  }else{
    cl <- NULL
  }
  
  
  pblapply(todo, function(X){
    d <- list(X[[1]][1])
    names(d) <- X[[1]][2]
    aggregateResult_old(opts, filtering = TRUE, verbose = 0, selected = d,
                        timestep = timestep, writeOutput = writeOutput, mcWeights = mcWeights, mcYears = mcYears)
    gc() # clean memory
  }, cl = cl)
  
  if(verbose == 1){
    cat("Start computing\n")
  }
  if(parallel){
    stopCluster(cl)
  }
  if(verbose == 1){
    cat("Mc all done\n")
  }
}



