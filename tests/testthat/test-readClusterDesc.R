# v710----

## Thermal ----
test_that("test read cluster", {
  path_study_test <- studyPathS
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # read clusters informations
  input <- readClusterDesc()
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
  
  # function setSimulationPath() provide areas names 
  areas <- opts_study_test$areasWithClusters
  
  # internal properties referential 
    # thermal only for v710
  ref_thermal <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"thermal"]
  ref_thermal <- ref_thermal[Version.Antares <= 
                               opts_study_test$antaresVersion | 
                               Version.Antares %in% NA]
  
  ### Properties output format ----
  test_that("Operating format (default)", {
    # read clusters informations
    input <- readClusterDesc()
    
    # test character "-" not exists
    testthat::expect_true(
      identical(grep(pattern = "-", x = names(input)), 
                integer(0)))
    
    # "-" is replaced by "." 
    testthat::expect_true("min.stable.power" %in% names(input))
    })
  
  test_that("Antares format", {
    # read clusters informations
    input <- readClusterDesc(dot_format = FALSE)
    
    # test character "-" exists
    testthat::expect_true(
      any(grepl(pattern = "-", x = names(input))))
  })
  
  ### function return value ----
  test_that("That's function return", {
    # read clusters informations
    input <- readClusterDesc()
    
    # tests
    testthat::expect_true("data.table" %in% class(input))
    testthat::expect_true(all(areas %in% unique(input$area)))
    testthat::expect_equal(class(input$cluster), "factor")
  })
  
  ### Default values ----
    # Default properties read (using referential)
    # it's independent of the format of column names
  test_that("All value must be display according to study version", {
    # read clusters informations
    input <- readClusterDesc()
    
    # "name" in .ini file corresponding "cluster" in output
    testthat::expect_true(all(
      setdiff(ref_thermal$operating_format, "name")%in%
        setdiff(colnames(input), c("area", "cluster"))))
  })
  
  ### Read properties ----
  test_that("Test properties read from .ini", {
    # read clusters informations
      # need .ini format 
    input <- readClusterDesc(dot_format = FALSE)
    
    area_test <- getAreas()[1]
    cluster_target <- "BASE"
    
    file_propertie_path <- file.path(opts_study_test$inputPath,
                                     "thermal", 
                                     "clusters",
                                     area_test,
                                     "list.ini")
    
    list_properties <- readIniFile(file_propertie_path)
    list_properties <- list_properties[[cluster_target]]
    
    # select taget values 
    one_line_select <- input[area%in%area_test & cluster%in%tolower(cluster_target)]
    
    # test .ini values
      # special case on "cluster" (tolower + as.factor) corresponding to "name" in .ini 
    testthat::expect_equal(
      tolower(list_properties$name), 
      as.character(one_line_select$cluster))
      # some numerical values
    testthat::expect_equal(list_properties$`marginal-cost`, one_line_select$`marginal-cost`)
    testthat::expect_equal(list_properties$co2, one_line_select$co2)
    })
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
  
  test_that("colnames ordered", {
    # id_cols is already sorted but it could be not
    id_cols <- c("area", "cluster", "group")
    
    input <- readClusterDesc(dot_format = FALSE)
    expect_false(is.unsorted(setdiff(names(input), id_cols)))
  })
})



# v830 ----
## Renewables ----
   # NOTE : only tests on renewable part 
test_that("test read cluster renewables", {
  path_study_test <- grep(pattern = "test_case_study_v870", 
                          x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # read clusters informations
  input <- readClusterResDesc()
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_res <- opts_study_test$areasWithResClusters
  
  # tests if all colnames are returned according to ref
  ref_res <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"renewable"]
  ref_res <- ref_res[Version.Antares <= 
                       opts_study_test$antaresVersion | 
                       Version.Antares %in% NA]
  
  ### Properties output format ----
  test_that("Operating format (default)", {
    # read clusters informations
    input <- readClusterResDesc()
    
    # test character "-" not exists
    testthat::expect_true(
      identical(grep(pattern = "-", x = names(input)), 
                integer(0)))
    
    # "-" is replaced by "." 
    testthat::expect_true("ts.interpretation" %in% names(input))
  })
  
  test_that("Antares format", {
    # read clusters informations
    input <- readClusterResDesc(dot_format = FALSE)
    
    # test character "-" exists
    testthat::expect_true(
      any(grepl(pattern = "-", x = names(input))))
  })
  
  ### function return value ----
  test_that("That's function return", {
    # read clusters informations
    input <- readClusterResDesc()
    
    # tests
    testthat::expect_true("data.table" %in% class(input))
    testthat::expect_true(all(areas_res %in% unique(input$area)))
    testthat::expect_equal(class(input$cluster), "factor")
  })
  
  ### Default values ----
  # Default properties read (using referential)
  # it's independent of the format of column names
  test_that("All value must be display according to study version", {
    # read clusters informations
    input <- readClusterResDesc()
    
    # "name" in .ini file corresponding "cluster" in output
    testthat::expect_true(all(
      setdiff(ref_res$operating_format, "name")%in%
        setdiff(colnames(input), c("area", "cluster"))))
  })
  
  ### Read properties ----
  test_that("Test properties read from .ini", {
    # read clusters informations
    # need .ini format 
    input <- readClusterResDesc(dot_format = FALSE)
    
    area_test <- areas_res[1]
    cluster_target <- "at_res_1"
    
    file_propertie_path <- file.path(opts_study_test$inputPath,
                                     "renewables", 
                                     "clusters",
                                     area_test,
                                     "list.ini")
    
    list_properties <- readIniFile(file_propertie_path)
    list_properties <- list_properties[[cluster_target]]
    
    # select taget values 
    one_line_select <- input[area%in%area_test & cluster%in%tolower(cluster_target)]
    
    # test .ini values
    # some numerical values
    testthat::expect_equal(list_properties$nominalcapacity, one_line_select$nominalcapacity)
    testthat::expect_equal(list_properties$`ts-interpretation`, one_line_select$`ts-interpretation`)
    
    })
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
  
  test_that("colnames ordered", {
    # id_cols is already sorted but it could be not
    id_cols <- c("area", "cluster", "group")
    
    input <- readClusterResDesc(dot_format = FALSE)
    expect_false(is.unsorted(setdiff(names(input), id_cols)))
  })
})

# v860 ----
## st-storage ----
test_that("test read cluster st-storage v860", {
  path_study_test <- grep(pattern = "test_case_study_v870", 
                          x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # read clusters informations
  input <- readClusterSTDesc()
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))

  # function setSimulationPath() provide areas names with st-storage clusters
  areas_st <- opts_study_test$areasWithSTClusters
  
  
  # tests if all colnames are returned according to ref
  ref_st <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"storage"]
  ref_st <- ref_st[ Version.Antares  <= 
                       opts_study_test$antaresVersion | 
                     Version.Antares  %in% NA]
  
  ### Properties output format ----
  test_that("Operating format (default)", {
    # read clusters informations
    input <- readClusterSTDesc()
    
    # test character "-" not exists
    testthat::expect_true(
      identical(grep(pattern = "-", x = names(input)), 
                integer(0)))
  })
  
  test_that("Antares format", {
    # read clusters informations
    input <- readClusterSTDesc(dot_format = FALSE)
    
    # test character "-" exists (actualy not in st-storages)
    testthat::expect_false(
      any(grepl(pattern = "-", x = names(input))))
  })
  
  ### function return value ----
  test_that("That's function return", {
    # read clusters informations
    input <- readClusterSTDesc()
    
    # tests
    testthat::expect_true("data.table" %in% class(input))
    testthat::expect_true(all(areas_st %in% unique(input$area)))
    testthat::expect_equal(class(input$cluster), "factor")
  })
  
  ### Default values ----
  # Default properties read (using referential)
  # it's independent of the format of column names
  test_that("All value must be display according to study version", {
    # read clusters informations
    input <- readClusterSTDesc()
    
    # "name" in .ini file corresponding "cluster" in output
    testthat::expect_true(all(
      setdiff(ref_st$operating_format, "name")%in%
        setdiff(colnames(input), c("area", "cluster"))))
  })
  
  ### Read properties ----
  test_that("Test properties read from .ini", {
    # read clusters informations
    # need .ini format 
    input <- readClusterSTDesc(dot_format = FALSE)
    
    area_test <- areas_st[1]
    cluster_target <- "at_st_batt"
    
    file_propertie_path <- file.path(opts_study_test$inputPath,
                                     "st-storage", 
                                     "clusters",
                                     area_test,
                                     "list.ini")
    
    list_properties <- readIniFile(file_propertie_path)
    list_properties <- list_properties[[cluster_target]]
    
    # select taget values 
    one_line_select <- input[area%in%area_test & cluster%in%tolower(cluster_target)]
    
    # test .ini values
    # some numerical values
    testthat::expect_equal(list_properties$reservoircapacity, one_line_select$reservoircapacity)
    testthat::expect_equal(list_properties$efficiency, one_line_select$efficiency)
    })
  
  test_that("colnames ordered", {
    # id_cols is already sorted but it could be not
    id_cols <- c("area", "cluster", "group")
    
    input <- readClusterDesc(dot_format = FALSE)
    expect_false(is.unsorted(setdiff(names(input), id_cols)))
  })
})

# read empty study ----
test_that("test when study has no cluster (empty)", {
  path_empty_study <- setup_study_empty(sourcedir_empty_study)
  opts_study_test <- setSimulationPath(path_empty_study, simulation = "input")
  
  testthat::expect_equal(readClusterDesc(), 
                         data.table::data.table()) 
})

# v920 ----
# st-storage ----

# mock study
areas <- c("fr", "be")
opts <- list(
  "inputPath" = tempdir(),
  "typeLoad"= "txt",
  "areaList" = areas,
  "antaresVersion" = 920
)

# properties
fr <- c(
  paste0("[",
        areas[1], "_", "myClust",
        "]"),
  "name = fr_myClust",
  "group = Battery",
  "penalize-variation-injection = false")

be <- c(
  paste0("[",
         areas[2], "_", "myClust",
         "]"),
  "name = be_myClust",
  "group = Battery",
  "efficiencywithdrawal = 0.5")
  
list_properties <- list(fr, be)  

# create dir with properties
dir_path <- file.path(tempdir(), "st-storage", "clusters", areas)
lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)


# write properties
lapply(1:2, function(x){
  writeLines(list_properties[[x]], 
             file.path(dir_path[x], "list.ini"))
})


## Properties output format ----
test_that("Operating format (default)", {
  # read clusters informations
  input <- readClusterSTDesc(opts = opts)
  
  # test character "-" not exists
  testthat::expect_true(
    identical(grep(pattern = "-", x = names(input)), 
              integer(0)))
})

test_that("Antares format", {
  # read clusters informations
  input <- readClusterSTDesc(dot_format = FALSE, opts = opts)
  
  # test character "-" exists
  testthat::expect_true(
    any(grepl(pattern = "-", x = names(input))))
})

# Read properties ----
test_that("Test properties read from .ini", {
  # character "-" is available in .ini files
  input <- readClusterSTDesc(dot_format = FALSE, opts = opts)

  area_test <- areas[1]
  cluster_target <- "fr_myClust"

  file_propertie_path <- file.path(opts$inputPath,
                                   "st-storage",
                                   "clusters",
                                   area_test,
                                   "list.ini")

  list_properties <- readIniFile(file_propertie_path)
  list_properties <- list_properties[[cluster_target]]
  
  # select taget values 
  one_line_select <- input[area%in%area_test & 
                             cluster%in%tolower(cluster_target)]
  
  # test .ini values
  # some numerical values
  testthat::expect_equal(list_properties$`penalize-variation-injection`, 
                         one_line_select$`penalize-variation-injection`)
  
  test_that("colnames ordered", {
    # id_cols is already sorted but it could be not
    id_cols <- c("area", "cluster", "group")
    
    input <- readClusterDesc(dot_format = FALSE)
    expect_false(is.unsorted(setdiff(names(input), id_cols)))
  })
})

# v930 ----
# st-storage ----

# mock study
areas <- "fr"
opts <- list(
  "inputPath" = tempdir(),
  "typeLoad"= "txt",
  "areaList" = areas,
  "antaresVersion" = 930
)

# properties
fr <- c(
  paste0("[",
         areas[1], "_", "myClust",
         "]"),
  "name = fr_myClust",
  "group = Battery",
  "allow-overflow = false")


list_properties <- list(fr)  

# create dir with properties
dir_path <- file.path(tempdir(), "st-storage", "clusters", areas)
lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)


# write properties
lapply(1, function(x){
  writeLines(list_properties[[x]], 
             file.path(dir_path[x], "list.ini"))
})


## Properties output format ----
test_that("Operating format (default)", {
  # read clusters informations
  input <- readClusterSTDesc(opts = opts)
  
  # test character "-" not exists
  testthat::expect_true(
    identical(grep(pattern = "-", x = names(input)), 
              integer(0)))
})

test_that("Antares format", {
  # read clusters informations
  input <- readClusterSTDesc(dot_format = FALSE, opts = opts)
  
  # test character "-" exists
  testthat::expect_true(
    any(grepl(pattern = "-", x = names(input))))
})

# Read properties ----
test_that("Test properties read from .ini", {
  # character "-" is available in .ini files
  input <- readClusterSTDesc(dot_format = FALSE, opts = opts)
  
  area_test <- areas[1]
  cluster_target <- "fr_myClust"
  
  file_propertie_path <- file.path(opts$inputPath,
                                   "st-storage",
                                   "clusters",
                                   area_test,
                                   "list.ini")
  
  list_properties <- readIniFile(file_propertie_path)
  list_properties <- list_properties[[cluster_target]]
  
  # select taget values 
  one_line_select <- input[area%in%area_test & 
                             cluster%in%tolower(cluster_target)]
  
  # test .ini values
  testthat::expect_equal(list_properties$`allow-overflow`, 
                         one_line_select$`allow-overflow`)
  
  test_that("colnames ordered", {
    # id_cols is already sorted but it could be not
    id_cols <- c("area", "cluster", "group")
    
    input <- readClusterDesc(dot_format = FALSE, opts = opts)
    expect_false(is.unsorted(setdiff(names(input), id_cols)))
  })
})