test_that("getSimOptionsAPI return antares version in right format", {
  # given
  fixtures <- list(

    # --- root
    "sim/info/general" = list(
      mode = "adequacy",
      #version = 930,
      name = "test",
      date = "today"
    ),

    "sim/about-the-study/parameters" = list(p = 1),
    "sim/about-the-study/areas" = "fr\nde\n@district1",
    "sim/about-the-study/links" = "fr - de",

    # --- path adequacy
    "sim/adequacy/mc-all&depth=1" = list(),
    "sim/adequacy/mc-ind&depth=1" = list("00001" = list()),
    "sim/ts-numbers&depth=1" = list(),

    # then
    "sim/adequacy/mc-all/links&depth=2" = list(),
    "sim/adequacy/mc-all/areas&depth=2" = list()
  )

  testthat::local_mocked_bindings(
    read_secure_json = function(url, ...) fixtures[[url]],
    .getSuccess = function(url, ...) any(startsWith(names(fixtures), url)),
    .scan_output_links_folder = function(...) c("fr-de"),
    .detect_areas_with_clusters = function(...) c("fr"),
    .get_available_output_variables = function(...) c("var1"),
    .readLinksDef = function(x) data.frame(link="fr-de", from="fr", to="de"),
    .package = "antaresRead"
  )

  # when
  res <- .getSimOptionsAPI(paths = list(version=930, simPath="sim"))

  # then
  expect_equal(res$antaresVersion, 930)
})
