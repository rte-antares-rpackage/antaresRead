context("Aliases")

describe("setAlias", {
  it("adds a new alias", {
    setAlias("test", "test desc", c("area", "LOAD"))
    expect_false(is.null(pkgEnv$varAliases$test))
    expect_equal(pkgEnv$varAliases$test, list(desc = "test desc", select = c("area", "LOAD")))
  })
  
  it ("initializes alias list", {
    # Remove list of aliases
    oldList <- pkgEnv$varAliases
    rm(list = "varAliases", envir = pkgEnv)
    
    setAlias("test", "test desc", c("area", "LOAD"))
    expect_false(is.null(pkgEnv$varAliases))
    expect_equal(length(pkgEnv$varAliases), 1)
    expect_false(is.null(pkgEnv$varAliases$test))
    expect_equal(pkgEnv$varAliases$test, list(desc = "test desc", select = c("area", "LOAD")))
    
    pkgEnv$varAliases <- oldList
  })
})

describe("showAliases", {
  it("shows a short description of all aliases", {
    expect_output(showAliases(), "renewable")
  })
  
  it("returns a data.frame", {
    expect_output(aliases <- showAliases(), "renewable")
    expect_is(aliases, "data.frame")
    expect_equal(nrow(aliases), length(pkgEnv$varAliases))
    expect_equal(names(aliases), c("name", "desc", "select"))
  })
  
  it("shows the full description of an alias", {
    expect_output(aliases <- showAliases("renewable"), "SOLAR")
    expect_is(aliases, "data.frame")
    expect_equal(nrow(aliases), 1)
    expect_equal(names(aliases), c("name", "desc", "select"))
  })
})
