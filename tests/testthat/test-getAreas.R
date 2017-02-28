#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function getAreas")

opts <- list(
  areaList = c("a", "b", "c", "aa"),
  districtList = c("d"),
  districtsDef = data.table(
    district = as.factor("d"), 
    area = as.factor(c("a", "b"))
  ),
  areasWithClusters = c("a", "c")
)

describe("getAreas", {
  it("returns all areas if no parameter", {
    expect_equal(getAreas(opts = opts), opts$areaList)
  })
  
  it("searches areas with regexpr", {
    expect_equal(getAreas("a", opts = opts), c("a", "aa"))
  })
  
  it("searches areas with exact name", {
    expect_equal(getAreas("a", regexpSelect = FALSE, opts = opts), "a")
  })
  
  it("filters areas with regexpr", {
    expect_equal(getAreas(exclude = "a", opts = opts), c("b", "c"))
  })
  
  it("filters areas with exact name", {
    expect_equal(getAreas(exclude = "a", regexpExclude = FALSE, opts = opts), c("b", "c", "aa"))
  })
  
  it("combines select and filter", {
    expect_equal(getAreas("a|b", exclude = "b", opts = opts), c("a", "aa"))
  })
  
  it("returns areas with clusters", {
    expect_equal(getAreas("a", withClustersOnly = TRUE, opts = opts), "a")
  })
  
  it("returns areas from district", {
    expect_equal(getAreas(districts = "d", opts = opts), c("a", "b"))
  })
})

describe("getDistricts", {
  it ("returns district list", {
    expect_equal(getDistricts(opts = opts), c("d"))
  })
})
