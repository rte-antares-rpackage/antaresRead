# v710----

## Thermal ----
test_that("Size is positive", {
  opts <- list(
    "inputPath"=tempdir(),
    "typeLoad"='not_api',
    "areaList" = c("fr","de"),
    "antaresVersion" = 880,
    "mcYears" = seq(1,1000),
    "timeIdMax" = 8736,
    "timeIdMin" = 1
  )
  
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
  
  clusters_fr_path <- file.path(tempdir(),"thermal", "clusters", "fr")
  suppressWarnings(dir.create(clusters_fr_path, recursive = TRUE, showWarnings = FALSE))
  writeLines(ini_clusters, file.path(clusters_fr_path,"list.ini"))
  
  clusters_st_de_path <- file.path(tempdir(),"st-storage", "clusters", "de")
  suppressWarnings(dir.create(clusters_st_de_path, recursive = TRUE, showWarnings = FALSE))
  writeLines(ini_clusters_st, file.path(clusters_st_de_path,"list.ini"))
  
  size <- .giveSize(opts = opts, clusters = "fr", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  size <- .giveSize(opts = opts, clustersST = "de", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 2 * 11 * 5.5) / 1024^2
  testthat::expect_equal(size, expected_size)
  
  size <- .giveSize(opts = opts, clusters = "fr", clustersST = "de", timeStep = "hourly", mcYears = "all")
  expected_size <- (8735 * 1000 * 3 * 11 * 5.5) / 1024^2 + (8735 * 1000 * 2 * 11 * 5.5) / 1024^2
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
