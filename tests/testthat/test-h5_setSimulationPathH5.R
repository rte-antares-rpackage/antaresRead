context("h5 : setSimulationPathH5")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  test_that("h5 : identical setSimulationPathH5", {
    if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
      skip_on_cran()
    
    identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
    expect_identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
  })
  
  test_that("h5 : Error no file", {
    if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
      skip_on_cran()
    
    expect_error(setSimulationPathH5("badfilename"),
                 "Invalid path argument. File not found. Must be a .h5 file or a repertory with .h5 file(s)", fixed=TRUE)
  })
  
  
}
