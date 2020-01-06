context("h5 : No data")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  test_that("h5 : no data", {
    skip_according_to_options()
    
    rhdf5::h5createFile("testnodata.h5")
    rhdf5::h5createGroup("testnodata.h5", "hourly")
    DF1 <-  .h5ReadAntares("testnodata.h5", areas = "all", links = "all", clusters = "all", districts = "all")
    rhdf5::h5closeAll()
    expect_true(length(DF1) == 0)
    unlink("testnodata.h5")
  })
}
