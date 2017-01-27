#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test study in a temporary folder
path <- tempdir()

untar("testdata/antares-test-study.tar.gz", exdir = path)

assign("studyPath", file.path(path, "test_case"), envir = globalenv())
assign("nweeks", 2, envir = globalenv())
