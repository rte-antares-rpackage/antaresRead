#Copyright © 2016 RTE Réseau de transport d’électricité

context("Functions .importOutput")

path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)

opts <- setSimulationPath(path_study_test,simulation="20240105-0934eco")
    
test_that(".importOutputForSTClusters is ok", {
    
    OutputForSTClusters <- .importOutputForSTClusters(
        areas="fr",
        timeStep="annual",
        showProgress=FALSE, 
        parallel=FALSE,
        opts=opts
    )
    
    expect_true(all(c("injection","level","withdrawal") %in% colnames(OutputForSTClusters)))
    expect_equal(nrow(OutputForSTClusters),1)
})
