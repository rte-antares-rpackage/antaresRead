#Copyright © 2016 RTE Réseau de transport d’électricité




# test_that("ST clusters importation is ok", {
#   path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)
#   opts <- setSimulationPath(path_study_test,simulation="20240105-0934eco")
# 
#   clustersST <- readAntares(clustersST="all",timeStep="annual",opts = opts)$clustersST
#   expect_true(all(opts$areasWithSTClusters %in% clustersST$area))
# 
#   clustersST_fr <- readAntares(clustersST="fr",timeStep="annual",opts = opts)$clustersST
#   expect_true("fr"==unique(clustersST_fr$area))
# })
