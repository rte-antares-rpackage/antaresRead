#Copyright © 2016 RTE Réseau de transport d’électricité

context("Functions digest")

skip("Functions use warnings -- tests skipped for now")

sapply(c(studyPathS[2], studyPathSV8), function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  test_that("Read digest is ok", {
    digest <- readDigestFile(opts)
    expect_equal(length(digest), 5)
    expect_is(digest$areas, "data.table")
    expect_is(digest$lin, "data.table")
    expect_is(digest$quad, "data.table")
  })
  
  test_that("Merge and write digest are ok", {
    digest1 <- digest2 <- readDigestFile(opts)
    digest2$areas <- head(digest2$areas, 2)
    
    digest <- mergeDigests(digest2, digest1)
    expect_equal(length(digest), 5)
    expect_is(digest$areas, "data.table")
    expect_is(digest$lin, "data.table")
    expect_is(digest$quad, "data.table")
    expect_equal(nrow(digest$areas), max(nrow(digest1$areas), nrow(digest2$areas)))
    
    suppressWarnings(writeDigest(digest2, opts))
    digest <- readDigestFile(opts)
    expect_equal(length(digest), 5)
    expect_equal(nrow(digest$areas), nrow(digest2$areas))
  })
})
