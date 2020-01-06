context("h5 : setSimulationPathH5")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  test_that("h5 : identical setSimulationPathH5", {
    skip_according_to_options()
    
    identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
    expect_identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
  })
  
  test_that("h5 : Error no file", {
    skip_according_to_options()
    
    expect_error(setSimulationPathH5("badfilename"),
                 "Invalid path argument. File not found. Must be a .h5 file or a repertory with .h5 file(s)", fixed=TRUE)
  })
  
  
}
