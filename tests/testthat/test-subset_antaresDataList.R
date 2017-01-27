#Copyright © 2016 RTE Réseau de transport d’électricité

context("Subsetting antaresDataList")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", links = "all", clusters = "all",
                      timeStep = "daily", mcYears = "all",
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
    mytimeIds <- 9
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
    mysubset <- subset(mydata, areas = myarea, timeIds = 9, mcYears = 1) 
    expect_true(nrow(mysubset$areas) == 1)
    expect_true(nrow(mysubset$links) == length(mylinks))
  })
  
  it ("check parameter ", {
    mydata <- readAntares(areas = "a", links = "all", mcYears = "all", showProgress = FALSE)
    expect_silent(subset(mydata, areas = myarea))
    expect_error(subset(mydata, areas = "feeRFE"))
    expect_error(subset(mydata, mcYears=2695),"McYear 2695 is not an McYear of this study")
    expect_error(subset(mydata, timeIds=15599),"timeId 15599 is not an timeId of this study")
  })
  
})
