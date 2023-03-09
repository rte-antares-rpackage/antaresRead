#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (binding constraints)")

sapply(studyPathSV8, function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  if (simOptions()$antaresVersion >= 840)
    test_that("Binding constraints importation is ok", {
      suppressWarnings(results <- readAntares(areas="all", bindingConstraints = TRUE))
      bindingConstraints_input <- readIni(file.path("input", "bindingconstraints", "bindingconstraints.ini"))
      expect_is(results$bindingConstraints, "data.table")
      expect_equal(nrow(results$bindingConstraints), 24 * 7 * nweeks * length(bindingConstraints_input))
    })

})
