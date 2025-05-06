#Copyright © 2016 RTE Réseau de transport d’électricité

context("Functions .importOutput")

path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)

opts <- setSimulationPath(path_study_test,simulation="20240105-0934eco")


test_that(".check_missing_output_files is ok", {
  
  links <- c("at - fr", "fr - it", "at - it")  
  args <- data.frame("link" = links,
                     "path" = file.path(opts$simPath, "economy", "mc-ind", "00001", "links", links, "values-annual.txt")
                     )
  output_missing_expected <- c(FALSE,FALSE,TRUE)  
  output_missing <- .check_missing_output_files(opts = opts, args = args)
  
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


test_that("Test the general behaviour of .format_api_aggregate_result()", {
    
  expect_null(.format_api_aggregate_result(res = NULL))
  
  # mc-ind
  endpoint_res <- data.table("area" = rep("fr",12 * 2),
                             "cluster" = c(rep("Nuclear",12), rep("Gas",12)),
                             "mcYear" = rep(1, 12 * 2),
                             "timeId" = as.numeric(rep(seq(1,12),2)),
                             "production" = round(rnorm(n = 12 * 2, mean = 1000, sd = 50)),
                             "NP Cost" = round(rnorm(n = 12 * 2, mean = 10000, sd = 100)),
                             "NODU" = round(rnorm(n = 12 * 2, mean = 10, sd = 2)),
                             "Profit - Euro" = round(rnorm(n = 12 * 2, mean = 80, sd = 5))
                             )  
  final_res <- .format_api_aggregate_result(res = endpoint_res)
  areas <- unique(final_res$area)
  clusters <- unique(final_res$cluster)
  
  expect_true(inherits(final_res$timeId, what = "integer"))
  expect_true(inherits(final_res$area, what = "factor"))
  expect_true(inherits(final_res$cluster, what = "factor"))
  expect_true(all(areas == tolower(areas)))
  expect_true(all(clusters == tolower(clusters)))
  expect_true("profit" %in% colnames(final_res))
  
  # mc-all
  endpoint_res <- data.table("area" = rep("fr",12 * 2),
                             "timeId" = as.numeric(rep(seq(1,12),2)),
                             "OP. COST EXP" = round(rnorm(n = 12 * 2, mean = 102, sd = 7.5)),
                             "OP. COST MAX" = round(rnorm(n = 12 * 2, mean = 160, sd = 10)),
                             "OP. COST MIN" = round(rnorm(n = 12 * 2, mean = 80, sd = 5)),
                             "OP. COST STD" = round(rnorm(n = 12 * 2, mean = 40, sd = 2.5))  
                             )
                             
  final_res <- .format_api_aggregate_result(res = endpoint_res)
  areas <- unique(final_res$area)
  
  expect_true(inherits(final_res$timeId, what = "integer"))
  expect_true(inherits(final_res$area, what = "factor"))
  expect_true(all(areas == tolower(areas)))
  expect_true(all(c("OP. COST", "OP. COST_max", "OP. COST_min", "OP. COST_std") %in% colnames(final_res)))
})
