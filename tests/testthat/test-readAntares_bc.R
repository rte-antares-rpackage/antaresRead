#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (binding constraints)")

sapply(studyPathSV8, function(studyPath){
  
  #suppress for horizon warning
  suppressWarnings(opts <- setSimulationPath(studyPath))
  
  if (simOptions()$antaresVersion >= 840)
    test_that("Binding constraints importation is ok", {
      suppressWarnings(results <- readAntares(areas="all", bindingConstraints = TRUE, timeStep = "daily"))
      if (!is.null(results$bindingConstraints)) results <- results$bindingConstraints
      expect_is(results, "data.table")
    })

})