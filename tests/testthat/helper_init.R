#Copyright © 2016 RTE Réseau de transport d’électricité

options("antaresRead.skip_h5_on_cran" = TRUE)
options("antaresRead.skip_h5" = TRUE)
options("antaresRead.skip_h5_on_travis" = TRUE)
options("antaresRead.skip_h5_on_appveyor" = TRUE)


# Copy the test study in a temporary folder

path0 <- tempdir()

sourcedir <- system.file("inst/testdata", package = "antaresRead")
testH5 <- TRUE
if(sourcedir == ""){ sourcedir <- system.file("testdata", package = "antaresRead")}

## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
## we test dev version locally, on travis and appveyor but not in CRAN
if (length(strsplit(packageDescription("antaresRead")$Version, "\\.")[[1]]) > 3) { 
  Sys.setenv("RunAllAntaresReadTests"="yes")
}
.runH5Test <- FALSE #Sys.getenv("RunAllAntaresReadTests") == "yes"

compareValue <- function(A, B, res = NULL){
  if(class(A)[3] == "list"){
    res <- c(res, sapply(c("areas", "links", "cluster", "districts"), function(x){
      if(!is.null(A[[x]]))
      {
        compareValue(A[[x]], B[[x]], res = res)}}))
    
  }else{
    res <- c(res,sapply(names(A), function(X){
      if(identical(A[[X]], B[[X]])){
        TRUE
      }else{
        if(class(A[[X]]) %in% c("integer", "numeric")){
          identical(as.numeric(A[[X]]), as.numeric(B[[X]]))
        } else if(class(A[[X]]) %in% c("character", "factor")){
          identical(as.character(A[[X]]), as.character(B[[X]]))
        } else {
          FALSE
        }
      }
    }))
  }
}

# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run. 
# The following "if" prevents errors at this step
if (sourcedir != "") {
  
  studies <- list.files(
    path = sourcedir,
    pattern = "^antares-test-study.*\\.tar\\.gz$"
  )
  
  studies_names <- basename(studies)
  studies_names <- sub("\\.tar\\.gz$", "", studies_names)
  
  for (s in seq_along(studies)) {
    dir.create(file.path(path0, studies_names[s]))
    untar(file.path(sourcedir, studies[s]), exdir = file.path(path0, studies_names[s]))
  }
  
  if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
    
    path_v6 <- file.path(path0, "antares-test-study-v6")
    opts <- setSimulationPath(file.path(path_v6, "/test_case"))
    suppressMessages({
      suppressWarnings({
        
        #On cran we have only 2 threads so nbCore <- 1  
        if(.runH5Test){
          nbCoresTestHelper <- 4
        }else{
          nbCoresTestHelper <- 1
        }
        writeAntaresH5(path = path_v6, 
                       misc = TRUE, thermalAvailabilities = TRUE,
                       hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                       linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE,
                       overwrite=TRUE, nbCores = nbCoresTestHelper)
      })
    })
    
    #if you change the tar file then you must also change this file
    # h5file <- "20190321-2217eco-test.h5"
    h5file <- "20180423-1734eco-test.h5"
    
    deprintize<-function(f){
      return(function(...) {capture.output(w<-f(...));return(w);});
    }
    
    silentf <- deprintize(showAliases)
    
    alias <- silentf()$name
    alias <- as.character(alias)
    
    
    
    timeStep <-  c("hourly", "daily", "weekly", "monthly", "annual")
    
    assign("silentf", silentf, envir = globalenv())
    assign("tpDir", path_v6, envir = globalenv())
    assign("pathF", file.path(path_v6, "/", h5file), envir = globalenv())
    assign("h5file", h5file, envir = globalenv())
    assign("alias", alias, envir = globalenv())
    assign("compareValue", compareValue, envir = globalenv())
    assign("timeStep", timeStep, envir = globalenv())
    assign("optsG", opts, envir = globalenv())
    
  } 
  
  assign(
    x = "studyPathS",
    value = file.path(path0, studies_names, "test_case"),
    envir = globalenv()
  )
  
  assign("nweeks", 2, envir = globalenv())
  assign("nmonths", 2, envir = globalenv())
  assign("firstDay", 113, envir = globalenv())
  assign("lastDay", 126, envir = globalenv())
}


##Source dir for V8

sourcedir_V8 <- system.file("inst/test_v8", package = "antaresRead")
if(sourcedir_V8 == ""){ sourcedir_V8 <- system.file("test_v8", package = "antaresRead")}


if(sourcedir_V8 != ""){
  
  studies <- list.files(
    path = sourcedir_V8,
    pattern = "^test_case_study_v8.*\\.tar\\.gz$"
  )
  
  studies_names <- basename(studies)
  studies_names <- sub("\\.tar\\.gz$", "", studies_names)
  
  for (s in seq_along(studies)) {
    dir.create(file.path(path0, studies_names[s]))
    untar(file.path(sourcedir_V8, studies[s]), exdir = file.path(path0, studies_names[s]))
  }
  assign(
    x = "studyPathSV8",
    value = file.path(path0, studies_names, "test_case"),
    envir = globalenv()
  )
}




skip_according_to_options <- function() {
  if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
    skip_on_cran()
  if (isTRUE(getOption("antaresRead.skip_h5")))
    skip("h5 test skipped")
  if (isTRUE(getOption("antaresRead.skip_h5_on_travis")))
    skip_on_travis()
  if (isTRUE(getOption("antaresRead.skip_h5_on_appveyor")))
    skip_on_appveyor()
}

pathAPI <- "http://localhost:8080/studies/antaresStd/"
