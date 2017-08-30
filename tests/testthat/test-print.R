context("print methods")

sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

  
describe("print.antaresDataTable", {
  data <- readAntares(timeStep="annual", showProgress = FALSE)
  it ("prints basic information", {
    expected_message <- sprintf("'antaresDataTable' object with dimension %s x %s",
                                nrow(data), ncol(data))
    expect_output(print(data), expected_message)
    expect_output(print(data), "Type: areas")
    expect_output(print(data), "Synthesis: TRUE")
  })
})

describe("print.antaresDataList", {
  data <- readAntares("all", "all", timeStep="annual", showProgress = FALSE)
  
  it("prints basic information", {
    expect_output(print(data), "'antaresDataList' object with elements areas and links")
  })
  
  it ("prints dimension of each element", {
    for (e in c("areas", "links")) {
      expected_message <- sprintf(".$%s (%s x %s)", e, nrow(data[[e]]), ncol(data[[e]]))
    }
  })
  
  it ("handles case when there is only one element", {
    data$links <-  NULL
    expect_output(print(data), "'antaresDataList' object with element areas")
  })
})

describe("print.simOptions", {
  it("prints basic information about the study", {
    expect_output(print(opts), "Simulation 'test'")
  })
})
})
