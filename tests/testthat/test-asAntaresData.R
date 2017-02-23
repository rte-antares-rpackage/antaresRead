context("Conversion as antaresData")

describe("as.antaresDataTable", {
  
  it("converts data.frames", {
    dt <- data.frame(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataTable(dt, TRUE, "hourly", "areas")
    expect_is(adt, "antaresDataTable")
  })
  
  it("converts data.tables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataTable(dt, TRUE, "hourly", "areas")
    expect_is(adt, "antaresDataTable")
  })
  
  it("does not change antaresDatatTables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataTable(dt, TRUE, "hourly", "areas")
    expect_identical(adt, as.antaresDataTable(adt))
  })
  
  it("throws an error if x is of another type", {
    expect_error(as.antaresDataTable(1))
  })
  
})

describe("as.antaresDataList", {
  
  it("converts data.frames", {
    dt <- data.frame(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas")
    expect_is(adt, "antaresDataList")
    expect_false(is.null(adt$areas))
  })
  
  it("converts data.tables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas")
    expect_is(adt, "antaresDataList")
    expect_false(is.null(adt$areas))
  })
  
  it("converts antaresDataTables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataTable(dt, TRUE, "hourly", "areas")
    adl <- as.antaresDataList(adt)
    expect_is(adl, "antaresDataList")
    expect_false(is.null(adl$areas))
  })
  
  it("converts lists of tables", {
    l <- list(areas = data.table(timeId = 1:10, area = "a", value = rnorm(10)))
    adl <- as.antaresDataList(l, TRUE, "hourly")
    expect_is(adl, "antaresDataList")
    expect_false(is.null(adl$areas))
  })
  
  it("does not change antaresDataLists", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas")
    expect_identical(adt, as.antaresDataList(adt))
  })
  
  it("throws an error if x is of another type", {
    expect_error(as.antaresDataList(1))
  })
  
})
