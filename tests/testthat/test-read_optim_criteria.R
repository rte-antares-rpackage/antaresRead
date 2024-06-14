context("readOptimCriteria")


sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath, 1)

describe("readOptimCriteria", {
  it("returns an antaresDataTable", {
    optimCrit <- readOptimCriteria(opts)
    expect_is(optimCrit, "antaresDataTable")
    expect_equal(attr(optimCrit, "type"), "optimCriteria")
    expect_equal(attr(optimCrit, "synthesis"), FALSE)
    expect_equal(attr(optimCrit, "timeStep"), "weekly")
  })
})
})
