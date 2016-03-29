context("Helper functions used by readOutput")

studyPath <- system.file("antares_test_case/output/20160324-1430eco-test", package = "antares")
opts <- setSimulationPath(studyPath, trace=0)

nodes <- readOutput(select = "LOAD", showProgress = FALSE, synthesis = FALSE)
nodes$day <- nodes$month <- nodes$hour <- NULL

for (timeStep in c("daily", "weekly", "monthly", "annual")) {
  
  test_that(sprintf(".aggregateByTimeStep works for %s time step", timeStep), {
    
    calc <- antares:::.aggregateByTimeStep(nodes, timeStep, opts)
    read <- readOutput(select = "LOAD", timeStep = timeStep, showProgress = FALSE, synthesis = FALSE)
    
    tmp <- merge(calc, read[, .(node, timeId, mcYear, read = LOAD)], by = c("node", "timeId", "mcYear"))
    
    expect_true(tmp[, all(read - LOAD == 0)])
  })
  
}
