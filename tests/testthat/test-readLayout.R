context("Function readLayout")

opts <- setSimulationPath(studyPath, 1)

describe("readLayout", {
  it("returns the correct structure", {
    l <- readLayout(opts)
    expect_true(all(c("areas", "links") %in% names(l)))
    expect_equal(names(l$areas), c("area", "x", "y", "color"))
    expect_equal(names(l$links), c("link", "from", "to", "x0", "y0", "x1", "y1"))
  })
})
