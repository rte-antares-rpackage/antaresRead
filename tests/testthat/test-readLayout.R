context("Function readLayout")

sapply(studyPathS, function(studyPath){

opts <- setSimulationPath(studyPath, 1)

describe("readLayout", {
  it("returns the correct structure", {
    l <- readLayout(opts)
    expect_true(all(c("areas", "links") %in% names(l)))
    expect_equal(names(l$areas), c("area", "x", "y", "color"))
    expect_equal(names(l$links), c("link", "from", "to", "x0", "y0", "x1", "y1"))
    
    l <- readLayout(list(opts, opts))
    expect_true(all(c("areas", "links") %in% names(l)))
    expect_equal(names(l$areas), c("area", "x", "y", "color"))
    expect_equal(names(l$links), c("link", "from", "to", "x0", "y0", "x1", "y1"))
    
  })
  
  districtDefFile <- file.path(opts$inputPath, "areas/sets.ini")
  
  it("still works when there is no district (#50)", {
    # Move the file where districts are defined and create an empty one
    file.rename(districtDefFile, paste0(districtDefFile, ".back"))
    cat("", file = districtDefFile)
    
    expect_silent(opts <- setSimulationPath(studyPath, 1))
    expect_silent(l <- readLayout(opts))
    expect_true(all(c("areas", "links") %in% names(l)))
    expect_equal(names(l$areas), c("area", "x", "y", "color"))
    expect_equal(names(l$links), c("link", "from", "to", "x0", "y0", "x1", "y1"))
    expect_null(l$districts)
    expect_null(l$districtLinks)
  })
  
  file.remove(districtDefFile)
  file.rename(paste0(districtDefFile, ".back"), districtDefFile)
  
})

})
