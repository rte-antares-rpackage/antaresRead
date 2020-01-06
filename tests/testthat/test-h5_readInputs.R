context("h5 : read inputs")

if(.requireRhdf5_Antares(stopP = FALSE) & .runH5Test){
  test_that("h5 : h5ReadBindingConstraints", {
    if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
      skip_on_cran()
    
    optsH5 <- setSimulationPathH5(tpDir, h5file)
    re1 <- h5ReadBindingConstraints(optsH5)
    re2 <- antaresRead::readBindingConstraints(opts)
    for(i in 1:length(re1)){
      re1[[i]]$values <- data.frame(re1[[i]]$values )
      re2[[i]]$values <- data.frame(re2[[i]]$values )
      
    }
    expect_true(identical(re1, re2))
  })
  
  
  test_that("h5 : h5ReadLayout", {
    if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
      skip_on_cran()
    
    optsH5 <- setSimulationPathH5(tpDir, h5file)
    re1 <- h5ReadLayout(optsH5)
    re2 <- antaresRead::readLayout(opts)
    
    for(i in 1:length(re1)){
      re1[[i]] <- data.frame(re1[[i]])
      re2[[i]] <- data.frame(re2[[i]])
    }
    expect_true(identical(re1, re2))
  })
  
  test_that("h5 : h5ReadClusterDesc", {
    if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
      skip_on_cran()
    
    optsH5 <- setSimulationPathH5(tpDir, h5file)
    re1 <- data.frame(h5ReadClusterDesc(optsH5))
    re2 <- data.frame(antaresRead::readClusterDesc(opts))
    expect_true(identical(re1, re2))
  })
  
}
