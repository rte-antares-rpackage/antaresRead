context("Helper functions used by readAntares")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath, trace=0)

nodes <- readAntares(select = "LOAD", showProgress = FALSE, synthesis = FALSE)
nodes$day <- nodes$month <- nodes$hour <- NULL

for (timeStep in c("daily", "weekly", "monthly", "annual")) {
  
  test_that(sprintf(".aggregateByTimeStep works for %s time step", timeStep), {
    
    calc <- antares:::.aggregateByTimeStep(nodes, timeStep, opts)
    read <- readAntares(select = "LOAD", timeStep = timeStep, showProgress = FALSE, synthesis = FALSE)
    
    tmp <- merge(calc, read[, .(node, timeId, mcYear, read = LOAD)], by = c("node", "timeId", "mcYear"))
    
    expect_true(tmp[, all(read - LOAD == 0)])
    
  })
  
}
