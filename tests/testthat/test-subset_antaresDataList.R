#Copyright © 2016 RTE Réseau de transport d’électricité

context("Subsetting antaresDataList")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", links = "all", clusters = "all",
                      timeStep = "daily", mcYears = "all",
                      showProgress = FALSE)

myarea <- "a"
mylinks <- getLinks(myarea)
firstTimeId<-unique(mydata$areas$timeId)[1]

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
    mytimeIds <- firstTimeId
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
    mysubset <- subset(mydata, areas = myarea, timeIds = firstTimeId, mcYears = 1) 
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
  
  it("filters with another table", {
    filterTable <- data.table(timeId = 1:2, mcYear = 1:2, value = c(2, 5))
    filteredData <- subset(mydata, filterTable)
    expect_true(all(filteredData$areas[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
    expect_true(all(filteredData$links[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
    expect_true(all(filteredData$clusters[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
  })
  
  it("filters with another table containing areas", {
    filterTable <- data.table(area = "a", timeId = 1:2, mcYear = 1:2, value = c(2, 5))
    filteredData <- subset(mydata, filterTable)
    expect_true(all(filteredData$areas[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
    expect_true(all(filteredData$links[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
    expect_true(all(filteredData$clusters[, paste(timeId, mcYear)] %in% c("1 1", "2 2")))
    expect_true(all(filteredData$areas[, area] == "a"))
    expect_true(all(filteredData$links[, link] %in% getLinks("a")))
    expect_true(all(filteredData$clusters[, area] == "a"))
  })
  
})
})
