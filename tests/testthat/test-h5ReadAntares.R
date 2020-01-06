context(".h5ReadAntares")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  sapply(pkgEnv$allCompute, function(X){
    test_that(paste0("Select : ", X, " timeStep : "),{
      skip_according_to_options()
      
      param1 <- list(path = pathF, areas = "a", mcYears = 1, select = X)
      param2 <- list(path = pathF, areas = "a", mcYears = 1)
      param2[[X]] <- TRUE
      testthat::expect_true(identical(do.call(.h5ReadAntares, param1),
                                      do.call(.h5ReadAntares, param2)))
    })
  })
  
  
  ##Test
  paramComparaison <- list(
    areasAll = list(areas = "all"),
    linksAll = list(links = "all"),
    clustersAll = list(clusters = "all"),
    districtsAll = list(districts = "all"),
    areasAllMc1 = list(areas = "all", mcYears = 1),
    linksAllMc1 = list(links = "all", mcYears = 1),
    clustersAllMc1 = list(clusters = "all", mcYears = 1),
    districtsAllMc1 = list(districts = "all", mcYears = 1),
    areasAllMcAll = list(areas = "all", mcYears = "all"),
    linksAllMcAll = list(links = "all", mcYears = "all"),
    clustersAllMcAll = list(clusters = "all", mcYears = "all"),
    districtsAllMcAll = list(districts = "all", mcYears = "all"),
    areasaMcAll = list(area = "a", mcYears = "all"),
    linksBCMcAll = list(links = "b - c", mcYears = "all"),
    clustersaMcAll = list(clusters = "a", mcYears = "all"),
    districtsABMcAll = list(districts = "a and b", mcYears = "all"),
    linksFolowIn = list(links = "all", select = "FLOW LIN."),
    areasSelectAll = list(areas = "all", select = "all"),
    linksSelectAll = list(links = "all", select = "all"),
    clusterSelectAll = list(clusters = "all", select = "all"),
    districtsSelectAll = list(districts = "all", select = "all"),
    allData = list(areas = "all", links = "all", clusters = "all", districts = "all"),
    allDataMc1 = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = 1),
    allDataMc2 = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = 2),
    allDataMcAll = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all"),
    hourly = list(areas = "all", links = "all", clusters = "all", districts = "all", timeStep = "hourly"),
    daily = list(areas = "all", links = "all", clusters = "all", districts = "all", timeStep = "daily"),
    weekly = list(areas = "all", links = "all", clusters = "all", districts = "all", timeStep = "weekly"),
    monthly = list(areas = "all", links = "all", clusters = "all", districts = "all", timeStep = "monthly"),
    annual = list(areas = "all", links = "all", clusters = "all", districts = "all", timeStep = "annual")
  )
  
  sapply(names(paramComparaison), function(Z){
    test_that(paste(Z), {
      skip_according_to_options()
      
      param1 <- paramComparaison[[Z]]
      param2 <- param1
      
      ##Silent
      param1$showProgress <- FALSE
      param2$perf <- FALSE
      
      ##End silent
      param2$path <- pathF
      
      DF1 <- suppressWarnings({do.call(readAntares, param1)})
      DF2 <- do.call(.h5ReadAntares, param2)
      expect_true(all(unlist(compareValue(DF1, DF2))))
    })
  })
  
  test_that("Show perf", {
    skip_according_to_options()
    
    param1 <- list(areas = "all")
    param2 <- param1
    
    ##Silent
    param1$showProgress <- FALSE
    param2$perf <- FALSE
    
    ##End silent
    param2$path <- pathF
    DF1 <- suppressWarnings({do.call(readAntares, param1)})
    DF2 <- do.call(.h5ReadAntares, param2)
    expect_true(all(unlist(compareValue( DF1,DF2))))
  })
  
  test_that("Show perf multi request", {
    skip_according_to_options()
    
    param1 <- list(areas = "all", links = "all")
    param2 <- param1
    
    ##Silent
    param1$showProgress <- FALSE
    param2$perf <- FALSE
    
    ##End silent
    param2$path <- pathF
    DF1 <- suppressWarnings({do.call(readAntares, param1)})
    DF2 <- do.call(.h5ReadAntares, param2)
    expect_true(all(unlist(compareValue( DF1,DF2))))
  })
  
  
  
  #Test alias request
  for(i in alias){
    paramComparaison[[i]] <- list(select = i)
  }
  
  
  #Test remove
  for(i in alias){
    var <- strsplit(as.character(silentf(i)$select[1]), ",")[[1]]
    var <- gsub("^ ", "",var) 
    for(j in var)
    {
      minus <- paste0("-", j)
      paramComparaison[[paste(i,minus)]] <- list(select = c(i, minus))
    }
  }
  
  cgtrl <- sapply("hourly", function(Z){
    ctrl <- sapply(names(paramComparaison), function(X){
      oldw <- getOption("warn")
      options(warn = -1)
      
      test_that(paste(X, Z), {
        skip_according_to_options()
        
        param1 <- paramComparaison[[X]]
        param1$timeStep <- Z
        param2 <- param1
        
        ##Silent
        param1$showProgress <- FALSE
        param2$perf <- FALSE
        
        ##End silent
        param2$path <- pathF
        DF1 <- suppressWarnings({do.call(readAntares, param1)})
        DF2 <- do.call(.h5ReadAntares, param2)
        if(!is(DF1, "antaresDataList"))
        {
          setorderv(DF1, getIdCols(DF1))
        }else{
          for(i in 1:length(DF1)){
            setorderv(DF1[[i]], getIdCols(DF1[[i]]))
          }
        }
        if(!is(DF2, "antaresDataList"))
        {
          setorderv(DF2, getIdCols(DF2))
        }else{
          for(i in 1:length(DF2)){
            setorderv(DF2[[i]], getIdCols(DF2[[i]]))
          }
        }
        expect_true(all(unlist(compareValue( DF1,DF2))))
      })
      invisible()
      options(warn = oldw)
    })
    invisible()
  })
  
  test_that("Bad path", {
    skip_according_to_options()
    
    expect_error(.h5ReadAntares("toto"), "File toto not exist.")
    
  })
  
}
