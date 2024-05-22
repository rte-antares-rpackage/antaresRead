test_that("First test", {
  
  con <- file.path(tempdir(),"testReadIniFile.ini")
  file.create(path = con)
  cluster_list_ini_content <- c("[zone1_nuclear]", "group = Other", "name = zone1_nuclear", "test = true", "",
                                "[92i]", "group = Other", "name = 92i", "test = true", "",
                                "[zone1_gas]", "group = Other", "name = zone1_gas", "test = false", "",
                                "[7983e]", "group = Other", "name = 7983e", "test = true", "",
                                "[zone1_coal]", "group = Other", "name = zone1_coal", "test = false", ""
                               )
  writeLines(text = cluster_list_ini_content, con =  con)
  
  clusters <- readIniFile(con)
  
  # Specific cases not converted : 92i and 7983e
  clusters_name <- sapply(clusters, "[[", "name")
  clusters_name <- unname(clusters_name)
  expect_identical(clusters_name, c("zone1_nuclear", "92i", "zone1_gas", "7983e", "zone1_coal"))
  
  # Boolean
  clusters_test <- sapply(clusters, "[[", "test")
  clusters_test <- unname(clusters_test)
  expect_true(class(clusters_test) == "logical")
})
