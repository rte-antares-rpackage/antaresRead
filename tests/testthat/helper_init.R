#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test study in a temporary folder

path0 <- tempdir()

sourcedir <- system.file("testdata", package = "antaresRead")


## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
## we test dev version locally, on travis and appveyor but not in CRAN
if (length(strsplit(packageDescription("antaresRead")$Version, "\\.")[[1]]) > 3) { 
  Sys.setenv("RunAllAntaresReadTests"="yes")
}

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
  
  studyPathS <- file.path(path0, studies_names, "test_case")
  
  nweeks <- 2
  nmonths <- 2 
  firstDay <- 113
  lastDay <- 126
  
}


##Source dir for V8

#sourcedir_V8 <- system.file("inst/test_v8", package = "antaresRead")
sourcedir_V8 <- system.file("test_v8", package = "antaresRead")
#if(sourcedir_V8 == ""){ sourcedir_V8 <- system.file("test_v8", package = "antaresRead")}


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
  
  studyPathSV8 <- file.path(path0, studies_names, "test_case")
}


# skip_according_to_options <- function() {
#   if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
#     skip_on_cran()
#   if (isTRUE(getOption("antaresRead.skip_h5")))
#     skip("h5 test skipped")
#   if (isTRUE(getOption("antaresRead.skip_h5_on_travis")))
#     skip_on_travis()
#   if (isTRUE(getOption("antaresRead.skip_h5_on_appveyor")))
#     skip_on_appveyor()
# }

pathAPI <- "http://localhost:8080/studies/antaresStd/"


# study empty ----
  # latest version of study 
  # empty study to test exceptions
sourcedir_empty_study <- system.file("test_empty_study", 
                                     package = "antaresRead")

setup_study_empty <- function(dir_path){
  studies <- list.files(dir_path, pattern = "\\.tar\\.gz$", 
                        full.names = TRUE)
  # choose pattern 
  studies <- studies[grep(x = studies, 
                          pattern = "empty_study_v870")] 
  
  # untar etude
  path_sty <- file.path(tempdir(), 
                        "study_empty_latest_version")
  untar(studies[1], exdir = path_sty) # version latest
  study_temp_path <- file.path(path_sty, "test_case")
  
  study_empty_latest_version <- file.path(path_sty,
                                          "test_case")
  return(study_empty_latest_version)
  }
