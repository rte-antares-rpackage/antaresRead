#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (ST clusters)")

path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)

opts <- setSimulationPath(path_study_test,simulation="20240105-0934eco")

test_that("ST clusters importation is ok", {
  clustersST <- readAntares(clustersST="all",timeStep="annual",opts = opts)$clustersST
  
  expect_true(all(c("injection","level","withdrawal") %in% colnames(clustersST)))
  expect_true(all(opts$areasWithSTClusters %in% clustersST$area))
  
  clustersST_fr <- readAntares(clustersST="fr",timeStep="annual",opts = opts)$clustersST
  
  expect_true(all(c("injection","level","withdrawal") %in% colnames(clustersST_fr)))
  expect_true("fr" %in% clustersST$area)
})
