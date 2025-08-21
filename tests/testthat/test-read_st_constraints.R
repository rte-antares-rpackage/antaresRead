

# v920 ----

# build structure directory
# input/st-storage/clusters/list.ini
# input/st-storage/series/{area}/{cluster}/{ts.txt}

# input/st-storage/constraints/{area}/{cluster}/additional-constraints.ini
# input/st-storage/constraints/{area}/{cluster}/rhs_{constaints}.txt


test_that("Check opts",{
  expect_error(
    read_storages_constraints(opts = list(version=920)),
    regexp = "opts does not inherit from class simOptions"
  )
})

test_that("Check minimal version",{
  # given
  opts_min <- list(
    "studyPath" = tempdir(),
    "inputPath" = file.path(tempdir(), "input"),
    "typeLoad"= "txt",
    "areaList" = "areas",
    "antaresVersion" = 910
  )
  class(opts_min) <- c("simOptions")

  # then
  expect_error(
    read_storages_constraints(opts = opts_min)
  )
})

test_that("Check structure of list",{

  # given
  # mock study
  areas <- c("fr", "be")
  opts <- list(
    "studyPath" = tempdir(),
    "inputPath" = file.path(tempdir(), "input"),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 920
  )

  class(opts) <- c("simOptions")

  # properties
  fr <- c(
    "[withdrawal-1]",
    "variable = withdrawal",
    "operator = equal",
    "hours = [1,3,5], [120,121,122,123,124,125,126,127,128]",
    "",
    "[withdrawal-2]",
    "variable = withdrawal",
    "operator = equal",
    "hours = [1,3,5], [120,121,122,123,124]"
  )

  be <- c(
    "[netting-1]",
    "variable = netting",
    "operator = less",
    "hours = [1, 168]",
    "",
    "[netting-2]",
    "variable = netting",
    "operator = less",
    "hours = [1, 167,168,169]"
  )

  list_properties <- list(fr, be)

  # TS (8760, 1 column)
  TS_VALUE <- matrix(2, 8760)

  # create dir with properties
  cluster_names <- c("storage_1", "storage_2")
  comb <- merge(areas, cluster_names)

  dir_path <- file.path(tempdir(),
                        "input",
                        "st-storage",
                        "constraints",
                        areas,
                        paste0(comb$x, "_", comb$y))

  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)


  # write properties fr
  path_fr <- grep(pattern = "fr", x = dir_path, value = TRUE)
  lapply(path_fr, function(x){
    writeLines(list_properties[[1]],
               file.path(x, "additional-constraints.ini"))
  })

  # write properties be
  path_be <- grep(pattern = "be", x = dir_path, value = TRUE)
  lapply(path_be, function(x){
    writeLines(list_properties[[2]],
               file.path(x, "additional-constraints.ini"))
  })

  # write TS fr
  names_constraints_fr <- c("withdrawal-1", "withdrawal-2")

  lapply(path_fr, function(x){
    lapply(names_constraints_fr, function(xts){
      write.table(TS_VALUE,
                  file = file.path(x,
                                   paste0("rhs_",
                                          xts,
                                          ".txt")),
                  row.names = FALSE,
                  col.names = FALSE)
    })
  })

  # write TS be
  names_constraints_be <- c("netting-1", "netting-2")

  lapply(path_be, function(x){
    lapply(names_constraints_be, function(xts){
      write.table(TS_VALUE,
                  file = file.path(x,
                                   paste0("rhs_",
                                          xts,
                                          ".txt")),
                  row.names = FALSE,
                  col.names = FALSE)
    })
  })

  # when
  full_st_constraints <- read_storages_constraints(opts = opts)

  # then

  # structure
  # area
  expect_equal(names(full_st_constraints), c("be", "fr"))

  # cluster
  clust_names <- unlist(lapply(full_st_constraints, names),
                        use.names = FALSE)
  expect_equal(clust_names,
               c("be_storage_1", "be_storage_2", "fr_storage_1", "fr_storage_2"))

  # properties + values
  struct_names <- unlist(
    lapply(full_st_constraints, function(x){
      lapply(x, names)}),
    use.names = FALSE)
  expect_equal(unique(struct_names), c("properties", "values" ))


  test_that("Check properties",{
    #given
    ini_prop <- lapply(seq(1, length(dir_path)), function(x){
      readIniFile(file = file.path(dir_path[x], "additional-constraints.ini"))
    })

    names(ini_prop) <- c("fr", "be")

    # then
    expect_equal(full_st_constraints$fr$fr_storage_1$properties, ini_prop$fr)
    expect_equal(full_st_constraints$be$be_storage_2$properties, ini_prop$be)
  })


  test_that("Check values",{
    # given

    # fr
    ts_fr <- lapply(names_constraints_fr, function(x){
      fread_antares(file = file.path(dir_path[1],
                                     paste0("rhs_",
                                            x,
                                            ".txt")),
                    opts = opts)
    })

    # then
    expect_true(all(
      file.exists(file.path(dir_path[1],
                                      paste0("rhs_",
                                             names_constraints_fr,
                                             ".txt")))))

    expect_equal(ts_fr, list(data.table(TS_VALUE), data.table(TS_VALUE)))
  })
  unlink(opts$inputPath, recursive = TRUE)
})


test_that("Check with cluster without time series",{
  # /!\ TS are not mandatory !

  # given
  # mock study
  areas <- c("fr", "be")
  opts <- list(
    "studyPath" = tempdir(),
    "inputPath" = file.path(tempdir(), "input"),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 920
  )

  class(opts) <- c("simOptions")

  # properties
  fr <- c(
    "[withdrawal-1]",
    "variable = withdrawal",
    "operator = equal",
    "hours = [1,3,5], [120,121,122,123,124,125,126,127,128]",
    "",
    "[withdrawal-2]",
    "variable = withdrawal",
    "operator = equal",
    "hours = [1,3,5], [120,121,122,123,124]"
  )

  be <- c(
    "[netting-1]",
    "variable = netting",
    "operator = less",
    "hours = [1, 168]",
    "",
    "[netting-2]",
    "variable = netting",
    "operator = less",
    "hours = [1, 167,168,169]"
  )

  list_properties <- list(fr, be)

  # TS (8760, 1 column)
  TS_VALUE <- matrix(2, 8760)

  # create dir with properties
  cluster_names <- c("storage_1", "storage_2")
  comb <- merge(areas, cluster_names)

  dir_path <- file.path(tempdir(),
                        "input",
                        "st-storage",
                        "constraints",
                        areas,
                        paste0(comb$x, "_", comb$y))

  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)


  # write properties fr
  path_fr <- grep(pattern = "fr", x = dir_path, value = TRUE)
  lapply(path_fr, function(x){
    writeLines(list_properties[[1]],
               file.path(x, "additional-constraints.ini"))
  })

  # write properties be
  path_be <- grep(pattern = "be", x = dir_path, value = TRUE)
  lapply(path_be, function(x){
    writeLines(list_properties[[2]],
               file.path(x, "additional-constraints.ini"))
  })

  # write TS fr
  names_constraints_fr <- c("withdrawal-1")

  lapply(path_fr, function(x){
    lapply(names_constraints_fr, function(xts){
      write.table(TS_VALUE,
                  file = file.path(x,
                                   paste0("rhs_",
                                          xts,
                                          ".txt")),
                  row.names = FALSE,
                  col.names = FALSE)
    })
  })

  # write TS be
  names_constraints_be <- c("netting-1", "netting-2")

  lapply(names_constraints_be, function(xts){
    write.table(TS_VALUE,
                file = file.path(path_be[1],
                                 paste0("rhs_",
                                        xts,
                                        ".txt")),
                row.names = FALSE,
                col.names = FALSE)
  })


  # when
  full_st_constraints <- read_storages_constraints(opts = opts)

  # then

  # structure
  # area
  expect_equal(names(full_st_constraints), c("be", "fr"))

  # cluster
  clust_names <- unlist(lapply(full_st_constraints, names),
                        use.names = FALSE)
  expect_equal(clust_names,
               c("be_storage_1", "be_storage_2", "fr_storage_1", "fr_storage_2"))

  # properties + values
  struct_names <- unlist(
    lapply(full_st_constraints, function(x){
      lapply(x, names)}),
    use.names = FALSE)
  expect_equal(unique(struct_names), c("properties", "values" ))

  test_that("Check wiyhout values",{
    # given

    # cluster without ts
    path_without_values <- file.path(path_be[2])

    # then
    expect_equal(full_st_constraints$be$be_storage_2$values,
                 data.table())
  })

  unlink(opts$inputPath, recursive = TRUE)
})
