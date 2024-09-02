

# context("Function aggregateResult")
# 
# skip("")
# 
# pathstd <- tempdir()
# 
# opts <- setSimulationPath(studyPathSV8)
# 
# 
# test_that("test parallel aggregate", {
#   
#   mc_all_path <- file.path(opts$simDataPath, "mc-all", "grid", "digest.txt")
#   
#   mdate_original <- file.mtime(mc_all_path)
#   
#   parAggregateMCall(opts, nbcl = 2, mcYears = c(1,2), verbose = 0)
#   
#   expect_true(file.exists(mc_all_path))
#   expect_false(file.mtime(mc_all_path) == mdate_original)
#   expect_true(file.size(mc_all_path) > 0)
#   
#   mdate_original <- file.mtime(mc_all_path)
#   
#   parAggregateMCall(opts, nbcl = 1, mcYears = c(1,2), verbose = 0)
#   
#   expect_true(file.exists(mc_all_path))
#   expect_false(file.mtime(mc_all_path) == mdate_original)
#   expect_true(file.size(mc_all_path) > 0)
#   
# })
  

  

  