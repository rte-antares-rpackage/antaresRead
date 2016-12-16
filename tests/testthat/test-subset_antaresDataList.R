#Copyright © 2016 RTE Réseau de transport d’électricité

context("Subsetting antaresDataList")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", links = "all", clusters = "all",
                      timeStep = "monthly", mcYears = "all",
                      showProgress = FALSE)

myarea <- "a"
mylinks <- getLinks(myarea)

describe("subset.antaresDataTable", {
  
  it("do nothing if no parameters have been specified ", {
    expect_equal(mydata, subset(mydata))
  })
  
  it("filters areas", {
    mysubset <- subset(mydata, areas = "a")
    expect_true(all(mysubset$areas$area %in% myarea))
    expect_true(all(mysubset$clusters$area %in% myarea))
    expect_true(all(mysubset$links$link %in% mylinks))
  })
  
  it ("filters time ids", {
    mytimeIds <- 2
    mysubset <- subset(mydata, timeIds = mytimeIds)
    expect_true(all(mysubset$areas$timeId %in% mytimeIds))
    expect_true(all(mysubset$links$timeId %in% mytimeIds))
    expect_true(all(mysubset$clusters$timeId %in% mytimeIds))
  })
  
  it ("filters monte-carlo scenarios", {
    mymcYears <- 2
    mysubset <- subset(mydata, mcYears = mymcYears)
    expect_true(all(mysubset$areas$mcYear %in% mymcYears))
    expect_true(all(mysubset$links$mcYear %in% mymcYears))
    expect_true(all(mysubset$clusters$mcYear %in% mymcYears))
  })
  
  it ("filters all three variables together", {
    mysubset <- subset(mydata, areas = myarea, timeIds = 1, mcYears = 1) 
    expect_true(nrow(mysubset$areas) == 1)
    expect_true(nrow(mysubset$links) == length(mylinks))
  })
})
