#Copyright © 2016 RTE Réseau de transport d’électricité

context("Functions .importOutput")

path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)

opts <- setSimulationPath(path_study_test,simulation="20240105-0934eco")


test_that(".check_output_files_existence is ok", {
  
  links <- c("at - fr", "fr - it", "at - it")  
  args <- data.frame("link" = links,
                     "path" = file.path(opts$simPath, "economy", "mc-ind", "00001", "links", links, "values-annual.txt")
                     )
  output_missing_expected <- c(FALSE,FALSE,TRUE)  
  output_missing <- .check_output_files_existence(opts = opts, args = args)
  
  expect_equal(output_missing,output_missing_expected)
})


test_that(".importOutputForClusters is ok", {
    
    OutputForClusters <- .importOutputForClusters(
        areas="fr",
        timeStep="annual",
        showProgress=FALSE, 
        parallel=FALSE,
        opts=opts
    )
    
    required_order_simulation_variables <- c("production","NP Cost","NODU","profit")
    
    order_simulation_variables <- colnames(OutputForClusters)[colnames(OutputForClusters) %in% required_order_simulation_variables] 
    
    expect_equal(order_simulation_variables,required_order_simulation_variables)
    expect_equal(nrow(OutputForClusters),1)
})


test_that(".importOutputForResClusters is ok", {
    
    OutputForResClusters <- .importOutputForResClusters(
        areas="fr",
        timeStep="annual",
        showProgress=FALSE, 
        parallel=FALSE,
        opts=opts
    )
    
    required_order_simulation_variables <- c("production")
        
    order_simulation_variables <-  colnames(OutputForResClusters)[colnames(OutputForResClusters) %in% required_order_simulation_variables] 
    
    expect_equal(order_simulation_variables,required_order_simulation_variables)
    expect_equal(nrow(OutputForResClusters),1)
})
    
test_that(".importOutputForSTClusters is ok", {
    
    OutputForSTClusters <- .importOutputForSTClusters(
        areas="fr",
        timeStep="annual",
        showProgress=FALSE, 
        parallel=FALSE,
        opts=opts
    )
    
    required_order_simulation_variables <- c("P.injection","P.withdrawal","levels")
    
    order_simulation_variables <- colnames(OutputForSTClusters)[colnames(OutputForSTClusters) %in% required_order_simulation_variables] 
    
    expect_equal(order_simulation_variables,required_order_simulation_variables)
    expect_equal(nrow(OutputForSTClusters),1)
})
