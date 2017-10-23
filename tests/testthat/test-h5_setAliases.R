context("h5 : set aliases")

if(requireNamespace("rhdf5")){
  test_that("h5 : set h5 alias", {
    .setAlliasH5()
    expect_true("Out_surplusClusters" %in% silentf()$name)
  })
}
