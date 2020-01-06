context("h5 : write data")

# for use travis in parallel
Sys.unsetenv("R_TESTS")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  tpDir2 <- gsub("[\\]", "/", tpDir)
  tptpDir <- file.path(tpDir2, "/tpDir")
  
  dir.create(tptpDir)
  test_that("h5 : write more than one studies mono thread", {
    skip_according_to_options()
    
    writeAntaresH5(path = tptpDir, timeSteps = "annual", 
                   writeAllSimulations = TRUE, nbCores = 1, opts = optsG)
    
  })
  VV <- utils::sessionInfo()
  DoPar <- as.numeric(paste0(VV$R.version$major, VV$R.version$minor))>34
  
  if(DoPar)
  {
    test_that("h5 : overwrite + alldata + multi-thread", {
      skip_on_cran()
      skip_according_to_options()
      
      writeAntaresH5(path = tptpDir, overwrite = TRUE, allData = TRUE, 
                     timeSteps = "annual", writeAllSimulations = TRUE, 
                     nbCores = 2, opts = optsG)
      filesTptpDir<-dir(tptpDir)
      expect_true(TRUE %in% grepl("h5", filesTptpDir))
      
    })
  }
  
  test_that("h5 : overwrite + removeVirtualAreas", {
    skip_according_to_options()
    
    writeAntaresH5(path = tptpDir, 
                   overwrite = TRUE, 
                   opts = optsG, 
                   timeSteps = "hourly",
                   removeVirtualAreas = TRUE,
                   storageFlexibility = "c", 
                   nbCores = 1)
    filesTptpDir<-dir(tptpDir)
    expect_true(TRUE %in% grepl("h5", filesTptpDir))
    
  })
  
  
  unlink(tptpDir, recursive = TRUE)
  
  test_that("h5 : Bad path", {
    skip_according_to_options()
    
    expect_error( writeAntaresH5(path='badPath'), "Folder badPath not found.")
    
  })
  
}
