#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntaresClusters")

sapply(studyPathS, function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  test_that("Clusters importation is ok", {
    clusters <- readAntaresClusters(clusters = "peak_must_run_partial", showProgress = FALSE)
    expect_is(clusters, "data.table")
    expect_true(!is.null(clusters$cluster))
    expect_equal(nrow(clusters), 24 * 7 * nweeks)
  })
  
  
  # Test that importation works for all time resolutions.
  for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
    expected_rows = switch(timeStep,
                           hourly = 24 * 7 * nweeks,
                           daily = 7 * nweeks,
                           weekly = nweeks,
                           monthly = nmonths,
                           annual = 1)
    
    test_that(sprintf("one can import cluster %s output", timeStep), {
      clusters <- readAntaresClusters(clusters = "peak_must_run_partial", showProgress = FALSE, timeStep = timeStep)
      expect_equal(nrow(clusters), expected_rows)
    })
  }
  
})
