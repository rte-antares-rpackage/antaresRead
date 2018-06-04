context("h5 : setSimulationPathH5")

if(requireNamespace("rhdf5") & .runThisTest){
  test_that("h5 : identical setSimulationPathH5", {
    identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
    expect_identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
  })
  
  test_that("h5 : Error no file", {
    expect_error(setSimulationPathH5("badfilename"),
                 "Invalid path argument. File not found. Must be a .h5 file or a repertory with .h5 file(s)", fixed=TRUE)
  })
  
  
}
