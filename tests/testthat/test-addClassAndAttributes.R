#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function .addClassAndAttributes")

describe(".addClassAndAttributes", {
  
  dt <- data.table(area = "a", timeId = 1:10, LOAD = rnorm(10))
  dt2 <- data.table(link = "a - b", timeId = 1:10, FLOW = rnorm(10))
  
  it("transforms a data.table in 'antaresDataTable", {
    adt <- .addClassAndAttributes(dt, TRUE, "hourly", opts = list(), type = "areas")
    expect_is(adt, "antaresDataTable")
    expect_equal(attr(adt, "synthesis"), TRUE)
    expect_equal(attr(adt, "timeStep"), "hourly")
    expect_equal(attr(adt, "opts"), list())
    expect_equal(attr(adt, "type"), "areas")
  })
  
  it("transforms a list in 'antaresDataList'", {
    l <- list(areas = dt, links = dt2)
    al <- .addClassAndAttributes(l, TRUE, "hourly", opts = list(), simplify = TRUE)
    expect_is(al, "antaresDataList")
    expect_equal(attr(al, "synthesis"), TRUE)
    expect_equal(attr(al, "timeStep"), "hourly")
    expect_equal(attr(al, "opts"), list())
  })
  
  it("sets correctly the class and type of each element of a list", {
    l <- list(areas = dt, links = dt2)
    al <- .addClassAndAttributes(l, TRUE, "hourly", opts = list(), simplify = TRUE)
    expect_is(al$areas, "antaresDataTable")
    expect_is(al$links, "antaresDataTable")
    expect_equal(attr(al$areas, "type"), "areas")
    expect_equal(attr(al$links, "type"), "links")
  })
  
  it("returns an antaresDataTable if x is a list with only one element", {
    l <- list(areas = dt)
    al <- .addClassAndAttributes(l, TRUE, "hourly", opts = list(), simplify = TRUE)
    expect_is(al, "antaresDataTable")
    expect_equal(attr(al, "type"), "areas")
  })
  
  it("always returns an antaresDataList if simplify = FALSE", {
    l <- list(areas = dt)
    al <- .addClassAndAttributes(l, TRUE, "hourly", opts = list(), simplify = FALSE)
    expect_is(al, "antaresDataList")
  })
  
})
