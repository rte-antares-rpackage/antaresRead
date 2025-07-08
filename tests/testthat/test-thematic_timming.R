
test_that("minimal version is v8.8", {
  # given
  areas <- c("fr", "be")
  opts <- list(
    "inputPath" = tempdir(),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 860
  )

  # then
  expect_error(
    getThematicTrimming(opts = opts)
  )

})

# v8.8 ----
test_that("All variables active", {
  # test getThematicTrimming() return all default columns according version

  # given
  areas <- c("fr", "be")
  opts <- list(
    "inputPath" = tempdir(),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880
  )

  # when
  read_thematic <- getThematicTrimming(opts = opts)

  # then
  # structure returned
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                              c("variables", "status_selection")))

  # check variables names according to antares version
  antares_version <- as.character(opts$antaresVersion)
  filter_vars_version <- pkgEnv$thematic[[antares_version]]

  # test if variables are all in output
  testthat::expect_true(all(filter_vars_version$col_name%in%
                              read_thematic$variables))
  # test status values
  testthat::expect_equal(object = unique(read_thematic$status_selection),
                         expected = "active")
})

test_that("All variables skiped", {
  # given
  areas <- c("fr", "be")
  opts <- list(
    "inputPath" = tempdir(),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    parameters=list(
      `variables selection` = list(
        selected_vars_reset = FALSE
      )
    )
  )

  # when
  read_thematic <- getThematicTrimming(opts = opts)

  # then
  # structure returned
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                              c("variables", "status_selection")))

  # check variables names according to antares version
  antares_version <- as.character(opts$antaresVersion)
  filter_vars_version <- pkgEnv$thematic[[antares_version]]

  # test if variables are all in output
  testthat::expect_true(all(filter_vars_version$col_name%in%
                              read_thematic$variables))
  # test status values
  testthat::expect_equal(object = unique(read_thematic$status_selection),
                         expected = "skip")
})


test_that("selection variables (+)", {
  # given
  areas <- c("fr", "be")
  opts <- list(
    "inputPath" = tempdir(),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    parameters=list(
      `variables selection` = list(
        selected_vars_reset = FALSE,
        `select_var +` = "RENW. 4",
        `select_var +` = "DENS",
        `select_var +` = "Profit by plant"
      )
    )
  )

  # when
  read_thematic <- getThematicTrimming(opts = opts)

  # then
  # structure returned
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                              c("variables", "status_selection")))

  # check variables names according to antares version
  antares_version <- as.character(opts$antaresVersion)
  filter_vars_version <- pkgEnv$thematic[[antares_version]]

  # test if variables are all in output
  testthat::expect_true(all(filter_vars_version$col_name%in%
                              read_thematic$variables))

  # check status values
  index <- names(opts$parameters$`variables selection`)%in%"select_var +"
  var_selection <- unlist(opts$parameters$`variables selection`[index], use.names = FALSE)

  check_index <- read_thematic$variables %in% var_selection
  check_values <- read_thematic$variables[check_index]

  testthat::expect_equal(check_values, var_selection)
  testthat::expect_true(all(read_thematic[check_index,]$status_selection%in%
                              "active"))
})


test_that("selection variables (-)", {
  # given
  areas <- c("fr", "be")
  opts <- list(
    "inputPath" = tempdir(),
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    parameters=list(
      `variables selection` = list(
        selected_vars_reset = FALSE,
        `select_var -` = "LOOP FLOW",
        `select_var -` = "FLOW QUAD.",
        `select_var -` = "CONG. FEE (ALG.)",
        `select_var -` = "CONG. FEE (ABS.)"
      )
    )
  )

  # when
  read_thematic <- getThematicTrimming(opts = opts)

  # then
  # structure returned
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                              c("variables", "status_selection")))

  # check variables names according to antares version
  antares_version <- as.character(opts$antaresVersion)
  filter_vars_version <- pkgEnv$thematic[[antares_version]]

  # test if variables are all in output
  testthat::expect_true(all(filter_vars_version$col_name%in%
                              read_thematic$variables))

  # check status values
  index <- names(opts$parameters$`variables selection`)%in%"select_var -"
  var_selection <- unlist(opts$parameters$`variables selection`[index], use.names = FALSE)

  check_index <- read_thematic$variables %in% var_selection
  check_values <- read_thematic$variables[check_index]

  testthat::expect_equal(check_values, var_selection)
  testthat::expect_true(all(read_thematic[check_index,]$status_selection%in%
                              "skip"))
})



