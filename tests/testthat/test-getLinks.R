# Copyright © 2019 RTE Réseau de transport d’électricité

context("Function getLinks")

opts <- list(
  areaList = c("at", "de", "ie", "es"),
  linksDef = data.table(
    link = c("de - ie", "es - de", "es - ie"),
    from = c("de", "es", "es"),
    to = c("ie", "de", "ie")
  )
)

describe("getLinks", {
  it("returns the list of links", {
    expect_equal(getLinks(opts = opts), c("de - ie", "es - de", "es - ie"))
  })
  
  it("works for an area with no links", {
    expect_equal(
      nrow(getLinks("at", opts = opts, namesOnly = FALSE, withDirection = TRUE)),
      0
    )
  })
})
