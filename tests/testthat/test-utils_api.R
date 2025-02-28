#Copyright © 2016 RTE Réseau de transport d’électricité
test_that("import only data related to some areas with 'select'", {
  
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", "", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", "v1/downloads", ""), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("","https://antares-web.rte-france.com/api", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", NULL, "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", "v1/downloads", NULL), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url(NULL,"https://antares-web.rte-france.com/api", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")

  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", NULL, "v1/downloads", ""), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url("https://antares-web.rte-france.com/api", NULL, "", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url(NULL,"https://antares-web.rte-france.com/api", NULL, "", "v1/downloads"), "https://antares-web.rte-france.com/api/v1/downloads")
  expect_equal(.generate_endpoint_url(NULL,"https://antares-web.rte-france.com/api", NULL, "", "v1/downloads", ""), "https://antares-web.rte-france.com/api/v1/downloads")
  
})
