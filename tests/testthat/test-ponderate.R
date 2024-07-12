# context("mcWeights")
# 
# skip("readAntares empty when mcYears is NULL")
# 
# setSimulationPath(studyPathSV8)
# 
# sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(tim){
#   correct_mc_weights <- 1:2
#   output_correct_weights_hourly <- readAntares(areas = "all", links = "all", clusters = "all", mcYears = "all", timeStep = tim, mcWeights = correct_mc_weights, showProgress = F)
#   output_synthetic_hourly <- readAntares(areas = "all", links = "all", clusters = "all", mcYears = NULL, timeStep = tim, showProgress = FALSE)
# 
#   ### Areas ###
#   output_weighted_areas <- output_correct_weights_hourly$areas
#   output_synthetic_areas <- output_synthetic_hourly$areas
#   data.table::setcolorder(output_weighted_areas, colnames(output_synthetic_areas))
# 
#   # output_weighted_areas <- output_weighted_areas[, round(.SD, 2), .SDcols = sapply(output_weighted_areas, is.numeric)]
#   # output_synthetic_areas <- output_synthetic_areas[, .SD, .SDcols = sapply(output_synthetic_areas, is.numeric)]
#   test <- output_synthetic_areas[, .SD, .SDcols = !getIdCols(output_weighted_areas)] - output_weighted_areas[, .SD, .SDcols = !getIdCols(output_weighted_areas)]
#   test <- round(test, 0)
#   test$PSP <- test$`MISC. NDG` <- NULL
#   non_null_cols <- colMeans(abs(test))
#   non_null_cols <- which(non_null_cols > 1)
#   test2 <- test[, .SD, .SDcols = non_null_cols]
#   expect_true(ncol(test2) == 0)
# 
#   ### Links ###
#   output_weighted_links <- output_correct_weights_hourly$links
#   output_synthetic_links <- output_synthetic_hourly$links
#   setcolorder(output_weighted_links, colnames(output_synthetic_links))
# 
#   output_weighted_links <- output_weighted_links[, round(.SD, 2), .SDcols = sapply(output_weighted_links, is.numeric)]
#   output_synthetic_links <- output_synthetic_links[, .SD, .SDcols = sapply(output_synthetic_links, is.numeric)]
#   test <- output_synthetic_links - output_weighted_links
#   test <- round(test, 0)
#   non_null_cols <- colMeans(abs(test))
#   non_null_cols <- which(non_null_cols > 20)
#   test2 <- test[, .SD, .SDcols = non_null_cols]
#   expect_true(ncol(test2) == 0)
# 
#   ### Clusters ###
#   output_weighted_clusters <- output_correct_weights_hourly$clusters
#   output_synthetic_clusters <- output_synthetic_hourly$clusters
#   setcolorder(output_weighted_clusters, colnames(output_synthetic_clusters))
# 
#   output_weighted_clusters <- output_weighted_clusters[, round(.SD, 2), .SDcols = sapply(output_weighted_clusters, is.numeric)]
#   output_synthetic_clusters <- output_synthetic_clusters[, .SD, .SDcols = sapply(output_synthetic_clusters, is.numeric)]
#   test <- output_synthetic_clusters - output_weighted_clusters
#   test <- round(test, 0)
#   non_null_cols <- colMeans(abs(test))
#   non_null_cols <- which(non_null_cols > 1)
#   test2 <- test[, .SD, .SDcols = non_null_cols]
#   expect_true(ncol(test2) == 0)
# 
# })
