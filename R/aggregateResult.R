#' @param nbcl \code{numeric} Number of parralel process
#' @param verbose \code{numeric} see logs (1) or not (0)
#'
#' @import data.table parallel
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom pbapply pblapply pboptions
#' @importFrom doParallel registerDoParallel
#'
#' 
#' @export
#' 
#' @rdname aggregatate_mc_all
#' 

library(data.table)
library(doParallel)
library(stringr)

transformLinkTable <- function(linkTable, RES_mode){
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

createDigestLinksLIN <- function(testPar, tmstp){
  #browser()
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
    machin = strsplit(as.character(digest[i,link]), " - ")
    result[`...To` == machin[[1]][2], machin[[1]][1] := ifelse(round(as.numeric(digest[i,"FLOW LIN."])) == 0,
                                                               1000000000000000,
                                                               round(as.numeric(digest[i,"FLOW LIN."])))]
    result[`...To` == machin[[1]][1], machin[[1]][2] := ifelse(-round(as.numeric(digest[i,"FLOW LIN."])) == 0,
                                                               1000000000000000,
                                                               -round(as.numeric(digest[i,"FLOW LIN."])))]
    result[`...To` == machin[[1]][1], machin[[1]][1] := "X"]
  }
  result[result == 0] <- "--"
  result[result == 1000000000000000] <- 0
  result[is.na(result)] <- "X"
  
  result
}

createDigestLinksQUAD <- function(testPar, tmstp){
  #browser()
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
    machin = strsplit(as.character(digest[i,link]), " - ")
    result[`...To` == machin[[1]][2], machin[[1]][1] := ifelse(round(as.numeric(digest[i,"FLOW QUAD."])) == 0,
                                                               1000000000000000,
                                                               round(as.numeric(digest[i,"FLOW QUAD."])))]
    result[`...To` == machin[[1]][1], machin[[1]][2] := ifelse(-round(as.numeric(digest[i,"FLOW QUAD."])) == 0,
                                                               1000000000000000,
                                                               -round(as.numeric(digest[i,"FLOW QUAD."])))]
    result[`...To` == machin[[1]][1], machin[[1]][1] := "X"]
  }
  result[result == 0] <- "--"
  result[result == 1000000000000000] <- 0
  result[is.na(result)] <- "X"
  
  result
}

createDigestAreasAnnual <- function(testPar, linkTable){
  digest <- testPar[["areas"]]
  cols_remove = c("timeId","time")
  cols_remove_2 = grep("_",names(digest),value = T)
  digest <- digest[, -c(cols_remove,cols_remove_2), with = F]
  digest <- cbind(digest[,1],
                  round(digest[,c(2,3)]),
                  round(digest[,4], 2),
                  round(digest[, !(1:4)]))

  digest <- rbind(linkTable, digest)
  cat("Digest : Ok\n")
  digest
}

createDigestAreasHourly <- function(testPar, linkTable){
  digest <- testPar[["areas"]]
  cols_remove = c("month","hour","timeId","time","day")
  cols_remove_2 = grep("_",names(digest),value = T)
  digest <- digest[, -c(cols_remove,cols_remove_2), with = F]
  setnames(digest, "MRG. PRICE", "mrgprice")
  digest <- digest[, "MRG. PRICE" := mrgprice/8736][, lapply(.SD, sum, na.rm=TRUE), by="area"][, mrgprice := NULL]
  digest <- digest[, unique(c("OV. COST", "OP. COST", "MRG. PRICE", names(digest))), with = F]
  digest <- cbind(digest[,1],
                  round(digest[,c(2,3)]),
                  round(digest[,4], 2),
                  round(digest[, !(1:4)]))
  
  digest <- rbind(linkTable, digest)
  cat("Digest (hourly) : Ok\n")
  digest
}

gridFolderCreation <- function(opts){
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
    lnks <- antaresRead::getLinks(namesOnly = F)
    lnks <- sapply(lnks, function(X){X <- strsplit(X, " - ")})
    dt_lnks <- data.table()
    for (lnk in lnks){
      dt_lnks <- rbind(dt_lnks, data.table(upstream = lnk[1], downstream = lnk[2]))
    }
    dt_lnks <- dt_lnks[order(upstream)][!(is.na(upstream) | is.na(downstream))]
    write.table(dt_lnks, file = paste0(opts$simDataPath, "/mc-all/grid/links.txt"), 
                row.names = F, quote = F, sep = "\t")
    ##thermal.txt
    cat("Grid folder : Ok\n")
  } else {cat("Grid folder : already exists\n")}
}

writeDigestFile <- function(opts, output, tmstp, linkTable){
  digest_file <- paste0(opts$simDataPath, "/mc-all/grid/digest.txt")
  ## première table et retours ligne
  write(x = "digest", file = digest_file)
  if (tmstp == "annual") {
    digesta <- createDigestAreasAnnual(output[[1]], linkTable)
  } else if (tmstp == "hourly") { digesta <- createDigestAreasHourly(output[[1]], linkTable)}
  first_table = data.table(VARIABLES = ncol(digesta) - 1, AREAS = nrow(digesta) - 2, LINKS = 0)
  write.table(first_table, file = digest_file, row.names = F, quote = F, sep = "\t", append = T)
  write(x = "", file = digest_file, append = T)

  
  ## Digest areas
  write.table(digesta, file = digest_file, row.names = F, quote = F, sep = "\t", append = T)
  write(x = "\n", file = digest_file, append = T)
  ## deuxieme table et retours ligne
  write(x = "digest", file = digest_file, append = T)
  first_table = data.table(VARIABLES = 0, AREAS = 0, LINKS = 0)
  write.table(first_table, file = digest_file, row.names = F, quote = F, sep = "\t", append = T)
  write(x = rep("\n",5), file = digest_file, append = T)
  
  ## Digest links LIN
  digesta <- createDigestLinksLIN(output[[1]], tmstp)
  write(x = "Links (FLOW LIN.)", file = digest_file, append = T)
  write(x = "\tFrom...", file = digest_file, append = T)
  write.table(digesta, file = digest_file, row.names = F, quote = F, sep = "\t", append = T)
  write(x = rep("\n",2), file = digest_file, append = T)
  
  ## Digest links QUAD
  digesta <- createDigestLinksQUAD(output[[1]], tmstp)
  write(x = "Links (FLOW QUAD.)", file = digest_file, append = T)
  write(x = "\tFrom...", file = digest_file, append = T)
  write.table(digesta, file = digest_file, row.names = F, quote = F, sep = "\t", append = T)
  
  ## add empty first column
  lines <- readLines(digest_file)
  write(x = paste0("\t",lines[1]), file = digest_file, sep = "\t")
  for (line in lines[-1]){
    write(x = paste0("\t",line), file = digest_file, sep = "\t", append = T)
  }
}

ParReadAntares <- function(mcYear, pth, type, 
                           areasselect, linksSelect, 
                           clustersSelect, clustersResSelect){
  antaresRead::setSimulationPath(pth, simulation = -1)
  dt <- antaresRead::readAntares(area = areasselect, links = linksSelect, 
                                 clusters = clustersSelect, clustersRes = clustersResSelect, 
                                 timeStep = type, simplify = FALSE, 
                                 mcYears = mcYear, showProgress = FALSE)
  dt
}

parAggregateMCall <- function(opts, 
                              nbcl = 8, 
                              verbose = 2, 
                              timestep = c("annual", "daily", "hourly", "monthly", "weekly"),
                              writeOutput = TRUE, # for ADPatch compatibility
                              mcWeights = NULL,
                              mcYears = NULL,
                              filtering = FALSE,
                              selected = NULL){
  
  
  # browser()
  
  resultat <- list()
  
  # Determiner les timestep dispo
  timestep_dispo <- c()
  fr_ind_path <- file.path(list.dirs(file.path(opts$simDataPath,"mc-ind"))[2],"areas","fr")
  if (dir.exists(fr_ind_path)){
    timestep_dispo <- unique(lapply(str_split(list.files(fr_ind_path), "-"), function(X){str_split(X[2],"\\.")[[1]][[1]]}))
  }
  
  timestep <- intersect(timestep, timestep_dispo)
  print(timestep)
  
  if (is.null(timestep)){
    cat("No data found")
    return (1)  
  }
  
  # Supprimer le dossier mc_all si exist
  if (dir.exists(file.path(opts$simDataPath,"mc-all"))){
    unlink(file.path(opts$simDataPath,"mc-all"), recursive = T)
  }
  
  for (tmstp in timestep){
    closeAllConnections()
    pathEtude <- opts$studyPath
    parallel <- ifelse(nbcl > 1, T, F)
    
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
    
    if(writeOutput == FALSE & length(tmstp)>1){
      stop("If you want data return you must choose a unique timestep")
    }
    
    
    if(verbose > 0)
    {
      try({
        pb <- txtProgressBar(style = 3)
      })
    }
    
    oldw <- getOption("warn")
    options(warn = -1)
    opts <- antaresRead::setSimulationPath(opts$simPath, simulation = -1)
    options(warn = oldw)
    
    # Version which readAntares
    linkTable <- try({
      data.table::fread(system.file("/format_output/tableOutput_aggreg.csv", package = "antaresRead"))},
      silent = TRUE
    )
    antaresRead:::.errorTest(linkTable, verbose, "\nLoad of link table")
    
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
      mcWeights <- rep(1, length(opts$mcYears))
    }
    
    if(length(mcWeights)!=length(numMc)){
      stop('length of mcWeights must be the same as mcYears')
    }
    
    gc()
    #Dynamic batch value
    batch = floor(((as.numeric(memuse::Sys.meminfo()[[2]])/(1024*1024*1024)) * 0.7)/2)
    if (batch > 1 & length(numMc)%%batch == 1) batch <- batch + 1
    cat("\nBatch :",batch,"\n")
    
    coef_div_mc_pond <- sum(mcWeights)
    coef_div_mc_pond_2 <- sum(mcWeights * mcWeights)
    
    #sapply on timeStep
    #allTyped <- c("annual", "daily", "hourly", "monthly", "weekly")
    #allTyped <- 'hourly'
    
    output <- sapply(tmstp, function(type, verbose)
    {
      
      antaresRead:::.addMessage(verbose, paste0("------- Mc-all : ", type, " -------"))
      
      try({
        
        # browser()
        # load first MC-year
        a <- Sys.time()
        
        oldw <- getOption("warn")
        options(warn = -1)
        
        if(!filtering)
        {
          clustersSelect <- clustersResSelect <- areasselect <- linksSelect <- "all"
        } else {
          if(is.null(selected)){
            areasselect <- antaresRead:::.getAreasToAggregate(opts, type)
            linksSelect <- antaresRead:::.getLinksToAggregate(opts, type)
            clustersSelect <- clustersResSelect <- areasselect
          } else {
            areasselect <- selected$areas
            linksSelect <- selected$links
            clustersSelect <- selected[["clusters"]]
            clustersResSelect <- selected[["clustersRes"]]
          }
        }
        #browser()
        tt <- sum(.Internal(gc(FALSE, TRUE, TRUE))[13:14])
        dta <- antaresRead::readAntares(area = areasselect,
                                        links = linksSelect,
                                        clusters = clustersSelect,
                                        clustersRes = clustersResSelect,
                                        timeStep = type,
                                        simplify = FALSE,
                                        mcYears = numMc[1],
                                        showProgress = FALSE)
        tt = sum(.Internal(gc(FALSE, FALSE, TRUE))[13:14]) - tt
        #print(tt, units = "Mb")
        options(warn = oldw)
        
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
            for (itm in names(dta)){
              if(!is.null(dta[[itm]])){
                if (itm %in% c("areas","links")){
                  struct[[itm]] <- dta[[itm]][,.SD, .SDcols = 1:SDcolsStartareas]
                } else {
                  struct[[itm]] <- dta[[itm]][,.SD, .SDcols = 1:SDcolsStartClust]
                }
              }
            }
            
            for (itm in names(struct)){
              if(type == "weekly" & (!is.null(struct[[itm]]))){
                struct[[itm]]$timeId <- as.numeric(substr(struct[[itm]]$time, nchar(as.character(struct[[itm]]$time[1]))-1,
                                                          nchar(as.character(struct[[itm]]$time[1]))))
              }
              if(!is.null(struct[[itm]]$day)){
                struct[[itm]]$day <- ifelse(nchar(struct[[itm]]$day) == 1,
                                            paste0("0", struct[[itm]]$day),
                                            as.character(struct[[itm]]$day))
              }
            }
            
            
            b <- Sys.time()
            #value structure
            value <- antaresRead:::.giveValue(dta, SDcolsStartareas, SDcolsStartClust)
            N <- length(numMc)
            
            W_sum = 0
            w_sum2 = 0
            mean_m = 0
            S = 0
            
            # browser()
            value <- lapply(value, function(X){antaresRead:::.creatStats(X, W_sum, w_sum2, mean_m, S, mcWeights[1])})
            
            btot <- as.numeric(Sys.time() - b)
            if(verbose>0)
            {
              try({
                antaresRead:::.progBar(pb, type, 1, N, coef)
              })
            }
            #sequentially add values
            if(N>1)
            {
              for (j in 1:ceiling(N/batch)){
                lst_idx = 0 
                left = ((j-1)*batch + 2)
                right = min((j*batch) + 1,N)
                if (parallel == TRUE){
                  closeAllConnections()
                  cl <- makeCluster(nbcl)
                  registerDoParallel(cl)
                  paropts <- list(preschedule=TRUE)
                  clusterSetRNGStream(cl, 123)
                  par_time <- Sys.time()
                  tt <- sum(.Internal(gc(FALSE, TRUE, TRUE))[13:14])
                  lst_dtaTP <- plyr::llply(left:right, ParReadAntares, .parallel = T, 
                                           .paropts = list(.options.snow = paropts), 
                                           pth = pathEtude, type = type, areasselect = areasselect,
                                           linksSelect = linksSelect, clustersSelect = clustersSelect,
                                           clustersResSelect = clustersResSelect)
                  tt = sum(.Internal(gc(FALSE, FALSE, TRUE))[13:14]) - tt
                  cat("\n")
                  #print(tt, units = "Mb")
                  cat("\n",Sys.time() - par_time)
                }
                
                for(i in left:right){
                  lst_idx = lst_idx + 1
                  a <- Sys.time()
                  
                  oldw <- getOption("warn")
                  options(warn = -1)
                  
                  
                  if (parallel == FALSE){
                    dtaTP <- antaresRead::readAntares(area = areasselect,
                                                      links = linksSelect,
                                                      clusters = clustersSelect,
                                                      clustersRes = clustersResSelect,
                                                      timeStep = type,
                                                      simplify = FALSE,
                                                      mcYears = numMc[i],
                                                      showProgress = FALSE)
                  } else { dtaTP <- lst_dtaTP[[lst_idx]]}
                  
                  
                  options(warn = oldw)
                  
                  aTot <- aTot + as.numeric(Sys.time() - a)
                  b <- Sys.time()
                  
                  valueTP <- antaresRead:::.giveValue(dtaTP, SDcolsStartareas, SDcolsStartClust)
                  
                  nmKeep <- names(valueTP)
                  # browser()
                  
                  valueTP <- lapply(names(valueTP), function(X){
                    
                    antaresRead:::.creatStats(valueTP[[X]], value[[X]]$W_sum, value[[X]]$w_sum2, value[[X]]$mean_m, value[[X]]$S , mcWeights[i])
                    
                  })
                  
                  names(valueTP) <- nmKeep 
                  
                  # valueTP <- mapply(function(X, Y){.creatStats(X, Y$W_sum, Y$w_sum2, Y$mean_m, Y$S , mcWeights[i])}, X = valueTP, Y = value, SIMPLIFY = FALSE)
                  
                  value$areas <- antaresRead:::.updateStats(value[["areas"]], valueTP[["areas"]])
                  value$links <- antaresRead:::.updateStats(value[["links"]], valueTP[["links"]])
                  value$clusters <- antaresRead:::.updateStats(value[["clusters"]], valueTP[["clusters"]])
                  value$clustersRes <- antaresRead:::.updateStats(value[["clustersRes"]], valueTP[["clustersRes"]])
                  
                  btot <- btot + as.numeric(Sys.time() - b)
                  if(verbose>0)
                  {
                    try({
                      antaresRead:::.progBar(pb, type, i, N, coef)
                    })
                  }
                  
                  lst_dtaTP[[lst_idx]] <- 0
                }
              }
              
              #Calcul of sd
              oldw <- getOption("warn")
              options(warn = -1)
              b <- Sys.time()
              
              coef_div_var = (coef_div_mc_pond )#- coef_div_mc_pond_2 / coef_div_mc_pond
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
            
            options(warn = oldw)
            
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
            antaresRead:::.addMessage(verbose, paste0("\nTime for reading data : ", round(aTot,1), " secondes"))
            antaresRead:::.addMessage(verbose, paste0("Time for calculating : ", round(btot,1), " secondes"))
          }, silent = TRUE)
          
          antaresRead:::.errorTest(dtaLoadAndcalcul, verbose, "Load data and calcul")
          
          #Write area
          allfiles <- c("values")
          
          if(writeOutput == FALSE){
            if(verbose>0)
            {
              antaresRead:::.progBar(pb, type, 1, 1, 1, terminate = TRUE)
            }
            
            return(antaresRead:::.formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct))
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
                    antaresRead:::.writeFileOut(dta = areastowrite, timestep = type, fileType = f,
                                                ctry = areasel, opts = opts, folderType = "areas", nbvar = nbvar,
                                                indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                                nomcair = areaSpecialFile$Name, unit = areaSpecialFile$Unit,
                                                nomStruct = kepNam,Stats = areaSpecialFile$Stats)
                    
                    
                  })
                }
              }
            }), silent = TRUE)
            
            antaresRead:::.errorTest(areaWrite, verbose, "Area write")
            
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
                  antaresRead:::.writeFileOut(dta = linkstowrite, timestep = type, fileType = f,
                                              ctry = linksel, opts = opts, folderType = "links", nbvar = nbvar,
                                              indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                              nomcair = linkSpecialFile$Name, unit = linkSpecialFile$Unit,
                                              nomStruct = kepNam,Stats = linkSpecialFile$Stats)
                })
              }
            }), silent = TRUE)
            
            antaresRead:::.errorTest(linkWrite, verbose, "Link write")
            
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
                  antaresRead:::.writeFileOut(dta = endClustctry, timestep = type, fileType = "details",
                                              ctry = ctry, opts = opts, folderType = "areas", nbvar = nbvar,
                                              indexMin = indexMin, indexMax = indexMax, ncolFix = ncolFix,
                                              nomcair = nomcair, unit = unit, nomStruct = nomStruct,Stats = Stats)
                }), silent = TRUE)
                antaresRead:::.errorTest(detailWrite, verbose, "Detail write")
              }
            }
          }
        }
      })
      
      #browser()
      antaresRead:::.addMessage(verbose, paste0("------- End Mc-all : ", type, " -------"))
      antaresRead:::.formatOutput( lapply(value, function(X)(Reduce(cbind, X))), struct)
    }, verbose = verbose, simplify = FALSE)
    
    
    if(verbose>0)
    {
      try({
        close(pb)
      })
    }
    # browser()
    if (tmstp == "annual" | (tmstp == "hourly" & !("annual" %in% timestep))){
      # Create grid folder
      gridFolderCreation(opts)
      
      # Transform linkTable for digest (compatible v8)
      RES_mode <- F
      if (opts$antaresVersion >= 810){
        if (opts$parameters$`other preferences`$`renewable-generation-modelling` == "clusters"){
          RES_mode <- T
        }
      }
      
      linkTable <- transformLinkTable(linkTable, RES_mode)
      
      # Create digest
      writeDigestFile(opts, output, tmstp, linkTable)
    }
    
    if(length(output)==1) resultat[[tmstp]] <- output[[1]]
    resultat[[tmstp]] <- output
    
    mc_all <- file.path(opts$simDataPath, "mc-all")
    file.rename(from = mc_all, str_replace(mc_all, "mc-all", paste0("mc-all-",tmstp)))
  }
  
  mc_all_hourly <- file.path(opts$simDataPath, "mc-all-hourly")
  if (file.exists(mc_all_hourly)){
    file.rename(from = mc_all_hourly, str_replace(mc_all_hourly, "mc-all-hourly", "mc-all"))
    mc_all <- file.path(opts$simDataPath, "mc-all")
  } else {
    mc_all <- file.path(opts$simDataPath, "mc-all")
    dir.create(mc_all)
  }
  
  mc_alls <- grep("mc-all-", list.dirs(opts$simDataPath, recursive = F), value = T)
  for (mc_all_step in mc_alls){
    t <- Sys.time()
    files <- list.dirs(mc_all_step, recursive = F)
    file.copy(files, mc_all, recursive = T)
    print(paste(mc_all_step,": Done"))
    print(Sys.time() - t)
  }
  
  unlink(mc_alls, recursive = T)
  

  resultat
}


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
    aggregateResult(opts, filtering = TRUE, verbose = 0, selected = d, 
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
#' @export
#' 
#' @rdname aggregatate_mc_all
#' 
#' @examples
#' \dontrun{
#'    aggregateResult(opts)
#'
#' }
#' 
aggregateResult <- function(opts, verbose = 1,
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
  
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  opts <- antaresRead::setSimulationPath(opts$simPath)
  options(warn = oldw)
  
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
    mcWeights <- rep(1, length(opts$mcYears))
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
    
    try({
      
      # browser()
      # load first MC-year
      a <- Sys.time()
      
      oldw <- getOption("warn")
      options(warn = -1)
      
      if(!filtering)
      {
        dta <- antaresRead::readAntares(area = "all", links = "all", clusters = "all",
                                        clustersRes = "all", timeStep = type, simplify = FALSE, 
                                        mcYears = numMc[1], showProgress = FALSE)
      } else {
        if(is.null(selected)){
          
          areasselect <- .getAreasToAggregate(opts, type)
          linksSelect <- .getLinksToAggregate(opts, type)
          dta <- antaresRead::readAntares(area = areasselect,
                                          links = linksSelect,
                                          clusters = areasselect,
                                          clustersRes = areasselect,
                                          timeStep = type,
                                          simplify = FALSE,
                                          mcYears = numMc[1],
                                          showProgress = FALSE)
        } else {
          dta <- antaresRead::readAntares(area = selected$areas,
                                          links = selected$links,
                                          clusters = selected[["clusters"]],
                                          clustersRes = selected[["clustersRes"]],
                                          timeStep = type,
                                          simplify = FALSE,
                                          mcYears = numMc[1],
                                          showProgress = FALSE)
          
        }
      }
      
      options(warn = oldw)
      
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
              
              oldw <- getOption("warn")
              options(warn = -1)
              
              if(!filtering)
              {
                dtaTP <- antaresRead::readAntares(area = "all", links = "all", clusters = "all", clustersRes = "all",
                                                  timeStep = type, simplify = FALSE, mcYears = numMc[i], showProgress = FALSE)
              } else {
                
                if(is.null(selected)){
                  
                  dtaTP <- antaresRead::readAntares(area = areasselect, 
                                                    links = linksSelect, 
                                                    clusters = areasselect, 
                                                    clustersRes = areasselect,
                                                    timeStep = type, simplify = FALSE, 
                                                    mcYears = numMc[i], showProgress = FALSE)
                } else{
                  dtaTP <- antaresRead::readAntares(area = selected$areas,
                                                    links = selected$links,
                                                    clusters = selected[["clusters"]],
                                                    clustersRes = selected[["clustersRes"]],
                                                    timeStep = type,
                                                    simplify = FALSE,
                                                    mcYears = numMc[i],
                                                    showProgress = FALSE)
                  
                }
              }
              
              options(warn = oldw)
              
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
            oldw <- getOption("warn")
            options(warn = -1)
            b <- Sys.time()
            
            coef_div_var = (coef_div_mc_pond )#- coef_div_mc_pond_2 / coef_div_mc_pond
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
          
          options(warn = oldw)
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



.formatOutput <- function(out, struct){
  
  # out <- lapply(value, function(X)(Reduce(cbind, X)))
  # browser()
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
#' Write ini file from list obtain by antaresRead:::readIniFile and modify by user
#'
#' @param listData \code{list}, modified list obtained by antaresRead:::readIniFile.
#' @param pathIni \code{Character}, Path to ini file.
#' @param overwrite logical, should file be overwritten if already exist?
#'
#' @examples
#'
#' \dontrun{
#' pathIni <- "D:/exemple_test/settings/generaldata.ini"
#' generalSetting <- antaresRead:::readIniFile(pathIni)
#' generalSetting$output$synthesis <- FALSE
#' writeIni(generalSetting, pathIni)
#' }
#'
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

