#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (ST clusters)")

path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)

suppressWarnings(opts <- setSimulationPath(path_study_test, simulation = "20240105-0934eco"))

test_that("ST clusters importation is ok", {
  
  clustersST <- readAntares(clustersST = "all", timeStep = "annual",opts = opts)
  expect_true(all(opts$areasWithSTClusters %in% clustersST$area))
  
  clustersST_fr <- readAntares(clustersST = "fr", timeStep = "annual", opts = opts)
  expect_true("fr" == unique(clustersST_fr$area))
})

  
test_that("ST clusters importation is OK", {
  nweeks_study <- 52
	output_cols <- c("P.injection", "levels", "P.withdrawal")
  clusters <- readAntaresSTClusters(clusters = "fr_st_other1", selected = output_cols[1:2], timeStep = "hourly", showProgress = FALSE, opts = opts)
  expect_is(clusters, "data.table")
  expect_true(!is.null(clusters$cluster))
  expect_equal(nrow(clusters), 24 * 7 * nweeks_study)
	expect_true(all(output_cols[1:2] %in% colnames(clusters)))
	expect_false(output_cols[3] %in% colnames(clusters))
})
  

test_that("ST clusters importation is OK for all time resolutions.", {
  nweeks_study <- 52 
  for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
    expected_rows = switch(timeStep,
                           hourly = 24 * 7 * nweeks_study,
                           daily = 7 * nweeks_study,
                           weekly = nweeks_study,
                           monthly = 12,
                           annual = 1)
    
    clusters <- readAntaresSTClusters(clusters = "fr_st_other1", showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(clusters), expected_rows)
  }
})


test_that("ST clusters importation is KO if clusters do not belong to the study output", {
  expect_warning(clusters <- readAntaresSTClusters(clusters = c("fake_one", "not_a_cluster"), timeStep = "hourly", showProgress = FALSE, opts = opts),
	  regexp = "do not exist in the simulation"
	)
	expect_is(clusters, "data.table")
  expect_true(nrow(clusters) == 0)
})
