context("h5 : write data")

if(requireNamespace("rhdf5")){
  tpDir2 <- gsub("[\\]", "/", tpDir)
  tptpDir <- paste0(tpDir2, "/tpDir")
  dir.create(tptpDir)
  
  test_that("h5 : write more than one studies mono thread", {
    writeAntaresH5(path = tptpDir,
                   timeSteps = "annual", writeAllSimulations = TRUE, nbCores = 1, 
                   opts = optsG)
    
  })
  unlink(tptpDir, recursive = TRUE)
  
  test_that("h5 : Bad path", {
    expect_error( writeAntaresH5('badPath'), "Folder badPath not found.")
    
  })
  
}
