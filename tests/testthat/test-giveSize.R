
test_that("Check the size of the object to get", {
  opts <- list(
    "inputPath"=tempdir(),
    "typeLoad"='not_api',
    "areaList" = c("fr","de", "be"),
    "antaresVersion" = 880,
    "mcYears" = seq(1,1000),
    "timeIdMax" = 8736,
    "timeIdMin" = 1
  )
  opts$parameters$`other preferences`$`renewable-generation-modelling` <- "clusters"
  
  # enabled = true 3*
  ini_clusters <- c(
    "[fr_cl_1]",
    "name = fr_cl_1",
    "unitcount = 120",
    "nominalcapacity = 2",
    "enabled = true",
    "",
    "[fr_cl_2]",
    "name = fr_cl_2",
    "unitcount = 2",
    "nominalcapacity = 5",
    "enabled = true",
    "",
    "[fr_cl_3]",
    "name = fr_cl_3",
    "unitcount = 20",
    "nominalcapacity = 48",
    "enabled = true",
    ""
  )
  
  ini_clusters_st <- c(
    "[de_st_1]",
    "name = de_st_1",
    "unitcount = 120",
    "nominalcapacity = 2",
    "enabled = true",
    "",
    "[de_st_2]",
    "name = de_st_2",
    "unitcount = 2",
    "nominalcapacity = 5",
    "enabled = false",
    "",
    "[de_st_3]",
    "name = de_st_3",
    "unitcount = 20",
    "nominalcapacity = 48",
    "enabled = true",
    ""
  )
  
  ini_clusters_res <- c(
    "[be_res_1]",
    "name = be_res_1",
    "group = Solar PV",
    "unitcount = 1",
    "nominalcapacity = 84560.000000",
    "ts-interpretation = production-factor",
    "",
    "[be_res_2]",
    "name = be_res_2",
    "group = Solar Thermal",
    "unitcount = 1",
    "nominalcapacity = 42500.000000",
    "ts-interpretation = production-factor",
    "",
    "[be_res_3]",
    "name = be_res_3",
    "group = Wind Offshore",
    "unitcount = 1",
    "nominalcapacity = 33875.000000",
    "ts-interpretation = production-factor",
    "",
    "[be_res_4]",
    "name = be_res_4",
    "group = Wind Onshore",
    "unitcount = 1",
    "nominalcapacity = 54160.000000",
    "ts-interpretation = production-factor",
    ""
  )
  
  clusters_fr_path <- file.path(tempdir(),"thermal", "clusters", "fr")
  suppressWarnings(dir.create(clusters_fr_path, recursive = TRUE, showWarnings = FALSE))
  writeLines(ini_clusters, file.path(clusters_fr_path,"list.ini"))
  
  clusters_st_de_path <- file.path(tempdir(),"st-storage", "clusters", "de")
  suppressWarnings(dir.create(clusters_st_de_path, recursive = TRUE, showWarnings = FALSE))
  writeLines(ini_clusters_st, file.path(clusters_st_de_path,"list.ini"))
  
  clusters_res_be_path <- file.path(tempdir(),"renewables", "clusters", "be")
  suppressWarnings(dir.create(clusters_res_be_path, recursive = TRUE, showWarnings = FALSE))
  writeLines(ini_clusters_res, file.path(clusters_res_be_path,"list.ini"))
  
  size <- .giveSize(opts = opts, clusters = "fr", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  size <- .giveSize(opts = opts, clustersST = "de", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 2 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  size <- .giveSize(opts = opts, clustersRes = "be", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 4 * 8 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  size <- .giveSize(opts = opts, clusters = "fr", clustersST = "de", clustersRes = "be", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2 + (8735 * 1000 * 2 * 11 * 5.5) / 1024^2 + (8735 * 1000 * 4 * 8 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  # enabled = true 2* enabled = false 1*
  ini_clusters <- c(
    "[fr_cl_1]",
    "name = fr_cl_1",
    "unitcount = 120",
    "nominalcapacity = 2",
    "enabled = true",
    "",
    "[fr_cl_2]",
    "name = fr_cl_2",
    "unitcount = 2",
    "nominalcapacity = 5",
    "enabled = true",
    "",
    "[fr_cl_3]",
    "name = fr_cl_3",
    "unitcount = 20",
    "nominalcapacity = 48",
    "enabled = false",
    ""
  )
  
  writeLines(ini_clusters, file.path(clusters_fr_path,"list.ini"))
  
  size <- .giveSize(opts = opts, clusters = "fr", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 2 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  # enabled empty 3*
  ini_clusters <- c(
    "[fr_cl_1]",
    "name = fr_cl_1",
    "unitcount = 120",
    "nominalcapacity = 2",
    "",
    "[fr_cl_2]",
    "name = fr_cl_2",
    "unitcount = 2",
    "nominalcapacity = 5",
    "",
    "[fr_cl_3]",
    "name = fr_cl_3",
    "unitcount = 20",
    "nominalcapacity = 48",
    ""
  )
  
  writeLines(ini_clusters, file.path(clusters_fr_path,"list.ini"))
  
  size <- .giveSize(opts = opts, clusters = "fr", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  # enabled empty 2* enabled = true 1*
  ini_clusters <- c(
    "[fr_cl_1]",
    "name = fr_cl_1",
    "unitcount = 120",
    "nominalcapacity = 2",
    "",
    "[fr_cl_2]",
    "name = fr_cl_2",
    "unitcount = 2",
    "nominalcapacity = 5",
    "",
    "[fr_cl_3]",
    "name = fr_cl_3",
    "unitcount = 20",
    "nominalcapacity = 48",
    "enabled = true",
    ""
  )
  
  writeLines(ini_clusters, file.path(clusters_fr_path,"list.ini"))
  
  size <- .giveSize(opts = opts, clusters = "fr", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
})
