context("bindingConstraints")


sapply(studyPathS, function(studyPath){

opts <- setSimulationPath(studyPath, 0)

describe("readBindingConstraints", {
  it("returns an object of class 'bindingConstraints'", {
    constraints <- readBindingConstraints(opts)
    expect_is(constraints, "bindingConstraints")
  })
})


describe("summary.bindingConstraints", {
  it ("returns a data.frame describing the constraints", {
    constraints <- readBindingConstraints(opts)
    sumConstraints <- summary(constraints)
    expect_is(sumConstraints, "data.frame")
    expect_true(all(c("enabled", "timeStep", "equation") %in% names(sumConstraints)))
  })
})


describe("Both operator", {
  it ("returns a data.frame describing the constraints", {
    constraints <- readBindingConstraints(opts)
    constraints[[1]]$operator <- "both"
    sumConstraints <- summary(constraints)
    expect_is(sumConstraints, "data.frame")
    expect_true(all(c("enabled", "timeStep", "equation") %in% names(sumConstraints)))
  })
})


})
