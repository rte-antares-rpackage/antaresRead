#Copyright © 2016 RTE Réseau de transport d’électricité


# >= v710----
context("Function readInputTS")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

test_that("Load importation works", {
  input <- readInputTS(load = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Thermal availabilities importation works", {
  input <- readInputTS(thermalAvailabilities = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Run of river importation works", {
  input <- readInputTS(ror = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Hydro storage importation works", {
  input <- readInputTS(hydroStorage = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Hydro storage maximum power importation works", {
  input <- readInputTS(hydroStorageMaxPower = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Wind importation works", {
  input <- readInputTS(wind = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Solar importation works", {
  input <- readInputTS(solar = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Misc importation works", {
  input <- readInputTS(misc = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Reserve importation works", {
  input <- readInputTS(reserve = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Link capacity importation works", {
  input <- readInputTS(linkCapacity = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("readInputTs must work if we change opts$timeIdMin and opts$timeIdMax", {
  input <- readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "hourly",
                       opts = opts)
  expect_is(input, "antaresDataTable")
  
  sumRorA <- input[ , .(sumProdRor = sum(ror)), by= .(area, tsId)]
  sumRorA[, ror := sumProdRor]
  sumRorA <- sumRorA[, ror, by= .(area, tsId)]
  
  inputA <- suppressWarnings(readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "annual",
                       opts = opts))
  
  inputA[, .(ror) , by= .(area, tsId)]
  
  expect_equal(sumRorA, inputA[, .(ror) , by= .(area, tsId)])

  optsNew <- opts
  optsNew$timeIdMin <- 3500
  optsNew$timeIdMax <- 4000
  
  inputAN <- suppressWarnings(readInputTS(ror = "all",
                        showProgress = FALSE,
                        timeStep = "annual",
                        opts = optsNew))
  
  inputH <- readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "hourly",
                       opts = optsNew)
  
  sumRorANew <- inputH[ , .(sumProdRor = sum(ror)), by= .(area, tsId)]
  sumRorANew[, ror := sumProdRor]
  sumRorANew <- sumRorANew[, ror, by= .(area, tsId)]
  
  expect_equal(inputAN[, .( ror) , by= .(area, tsId)], sumRorANew)
  
})

})

# read latest version study
path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

# >= v860----

test_that("readInputTs mingen file v860", {
  
  # to read 8760
  opts_study_test$timeIdMax <- 8760
  
  # read mingen file of study 
  mingen_TS <- readInputTS(mingen = "all", opts = opts_study_test, timeStep = "hourly")
  
  # test class object
  testthat::expect_true("data.table" %in% class(mingen_TS))
  testthat::expect_true("antaresDataTable" %in% class(mingen_TS))
  
  
  # select one area 
  area_mingen <- unique(mingen_TS$area)[1]
  
  # dimension of mingen file for this area
  dim_file <- max(mingen_TS[area %in% area_mingen, tsId])
  
  # check dim file
  path_file <- file.path(opts_study_test$inputPath, "hydro", "series", area_mingen, "mingen.txt")
  
  nb_col <- dim(antaresRead:::fread_antares(file = path_file, opts = opts_study_test))[2]
  
  # check similar dim
  testthat::expect_equal(dim_file, nb_col)
  
})

# >= v920----
# mock study
areas <- c("fr", "be")

opts <- list(
  "inputPath" = tempdir(),
  "typeLoad"= "txt",
  "areasWithSTClusters" = areas,
  "timeIdMin" = 1,
  "timeIdMax" = 8736,
  "antaresVersion" = 920
)

# TS are read in "input/st-storage/series/{areas}/{cluster_name}"
  # NO RHS, only ratio values {0;1} and dim (n=8760, p=1)
list_value_920 <- c("cost-injection", 
                   "cost-withdrawal", 
                   "cost-level", 
                   "cost-variation-injection", 
                   "cost-variation-withdrawal")

list_value_920txt <-  paste0(list_value_920, 
                          ".txt")

# add_prefix = FALSE 
name_cluster <- "my_clust"

path_ts <- file.path(opts$inputPath, 
                     "st-storage",
                     "series",
                     areas, 
                     name_cluster)

path_ts <- lapply(path_ts, 
                  file.path, 
                  list_value_920txt)

path_ts <- unlist(path_ts)

ts_values <- matrix(0.7, 8760)

# create dir with properties
dir_path <- file.path(tempdir(), "st-storage", "series", areas, name_cluster)
lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)

# write matrix/series 
lapply(path_ts, function(x){
  write.table(x = ts_values, 
              file = x, 
              row.names = FALSE, 
              col.names = FALSE)
})

test_that("st-storage importation works", {
  input <- readInputTS(st_storage = "all", 
                       showProgress = FALSE, 
                       opts = opts)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * 52), 0)
  expect_true(all(
    list_value_920 %in% unique(input$name_file)))
})

# >= v930 ----
areas <- c("fr", "be")

opts <- list(
  "inputPath"       = tempdir(),
  "typeLoad"        = "txt",
  "areasWithSTClusters" = areas,
  "timeIdMin"       = 1,
  "timeIdMax"       = 8736,
  "antaresVersion"  = 930
)

list_value_930 <- c(
  "cost-injection",
  "cost-withdrawal",
  "cost-level",
  "cost-variation-injection",
  "cost-variation-withdrawal"
)
list_value_930txt <- paste0(list_value_930, ".txt")

name_cluster <- "my_clust"

# file paths
path_ts <- file.path(
  opts$inputPath, "st-storage", "series", areas, name_cluster
)
path_ts <- unlist(lapply(path_ts, file.path, list_value_930txt))

# matrix (8760, N)
N <-3
ts_values <- matrix(0.7, nrow = 8760, ncol = N)

# create directories
dir_path <- file.path(tempdir(), "st-storage", "series", areas, name_cluster)
lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)

# write matrix/series
lapply(path_ts, function(x) {
  write.table(ts_values, file = x, row.names = FALSE, col.names = FALSE)
})

test_that("st-storage importation works (8736 x N)", {
  input <- readInputTS(st_storage = "all", showProgress = FALSE, opts = opts)
  
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  
  # 1) time horizon = 8736
  expect_equal(max(input$timeId) - min(input$timeId) + 1, 8736)
  
  # 2) we have N columns (verified via the number of rows per series)
  # each (area, cluster, name_file) must have 8736 * N rows
  dt <- data.table::as.data.table(input)
  rows_per_ts <- dt[, .N, by = .(area, cluster, name_file)]
  expect_true(all(rows_per_ts$N == 8736 * N))
  expect_equal(unique(rows_per_ts$N / 8736), N)
  
  # 3) expected files present
  expect_true(all(list_value_930 %in% unique(input$name_file)))
})
