context("h5 : write data")

# for use travis in parallel
Sys.unsetenv("R_TESTS")

if(requireNamespace("rhdf5")){
  tpDir2 <- gsub("[\\]", "/", tpDir)
  tptpDir <- file.path(tpDir2, "/tpDir")
  
  dir.create(tptpDir)
  test_that("h5 : write more than one studies mono thread", {
    writeAntaresH5(path = tptpDir, timeSteps = "annual", 
                   writeAllSimulations = TRUE, nbCores = 1, opts = optsG)
    
  })
  VV <- utils::sessionInfo()
  DoPar <- as.numeric(paste0(VV$R.version$major, VV$R.version$minor))>34
  
  if(DoPar)
  {
    test_that("h5 : overwrite + alldata + multi-thread", {
      writeAntaresH5(path = tptpDir, overwrite = TRUE, allData = TRUE, 
                     timeSteps = "annual", writeAllSimulations = TRUE, 
                     nbCores = 2, opts = optsG)
      
    })
  }
  
  test_that("h5 : overwrite + removeVirtualAreas", {
    writeAntaresH5(path = tptpDir, overwrite = TRUE, opts = optsG, timeSteps = "hourly",removeVirtualAreas = TRUE,
                   storageFlexibility = "a")
    
  })
  
  
  unlink(tptpDir, recursive = TRUE)
  
  test_that("h5 : Bad path", {
    expect_error( writeAntaresH5(path='badPath'), "Folder badPath not found.")
    
  })
  
}
