#Copyright © 2016 RTE Réseau de transport d’électricité

context("Object antaresDataTable")

l <- list.files()

opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", showProgress = FALSE)

test_that("subsetting an antaresDataTable returns an antaresDataTable", {
  subsetdt <- mydata[area == "a"]
  expect_is(subsetdt, "antaresDataTable")
  expect_true(all(c("timeStep", "opts", "synthesis", "type") %in% names(attributes(subsetdt))))
})

test_that("modifying columns of an antaresDataTable returns an antaresDataTable", {
  newdata <- copy(mydata)
  newdata[, load2 <- LOAD * 2]
  newdata[, LOAD := NULL]
  expect_is(newdata, "antaresDataTable")
  expect_true(all(c("timeStep", "opts", "synthesis", "type") %in% names(attributes(newdata))))
})

test_that("merge an antaresDataTable with a table returns an antaresDataTable", {
  newdata <- merge(mydata, opts$districtsDef, by = "area")
  expect_is(newdata, "antaresDataTable")
  expect_true(all(c("timeStep", "opts", "synthesis", "type") %in% names(attributes(newdata))))
})
