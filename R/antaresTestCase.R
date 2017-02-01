antaresTestCase <- function(dir = tempdir()) {
  path <- system.file("extdata", "antares_test_case.tar.gz", package = "antaresRead")
  
  print(list.files(system.file(package = "antaresRead")))
  untar(path, exdir = dir)
  return(file.path(dir, "test_case"))
}
