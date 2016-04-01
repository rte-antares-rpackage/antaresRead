# Copy the test study in a temporary file and run a simulation if it has not
# been already done.

if (!exists("studyPath")) {

  path <- tempdir()
  
  file.copy(system.file("test_case", package="antares"), path, recursive = TRUE, overwrite = TRUE)
  
  cmd <- '"C:/Program Files/RTE/Antares/5.0.0/bin/antares-5.0-solver.exe" "%s" -n "test"'
  
  cmd <- sprintf(cmd, file.path(path, "test_case"))
  
  system(cmd, ignore.stdout = TRUE)
  
  assign("studyPath", file.path(path, "test_case"), envir = globalenv())
} 
