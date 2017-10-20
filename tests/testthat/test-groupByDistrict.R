context("Function .groupByDistrict")


sapply(studyPathS, function(studyPath){

opts <- setSimulationPath(studyPath)
  
describe(".groupByDistrict", {

  it("groups data by district", {
    mydata <- readAntares(c("a", "b"), select = "LOAD", timeStep = "monthly", showProgress = FALSE)
    mydataAgg <- .groupByDistrict(mydata, opts)
    expect_true(is.null(mydataAgg$area))
    expect_false(any(duplicated(mydataAgg[, .(district, timeId, time)])))

    expect_identical(mydataAgg$LOAD,
                     mydata[area=="a", LOAD] + mydata[area=="b", LOAD])
  })

  it("throws a warning if an area is missing", {
    mydata <- readAntares("a", select = "LOAD", timeStep = "monthly", showProgress = FALSE)
    expect_warning(.groupByDistrict(mydata, opts), " b$")
  })
})
})
