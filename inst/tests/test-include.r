context("Include")

roxy_dir <- function(path, behaviour = no_output()) {
  bundle <- DirectoryBundle(path, behaviour)
  blocks <- process(bundle)@blocks
  blocks[[length(blocks)]]@tags
}

test_that("included files come earlier", {
  out <- roxy_dir("include-clothes")
  collate <- out$DESCRIPTION$Collate

  before <- function(a, b) {
    all(which(collate %in% a) < which(collate %in% b))
  }

  expect_true(before(c("pants", "shirt"), "belt"))
  expect_true(before(c("tie", "belt"), "jacket"))
  expect_true(before("undershorts", "pants"))
  expect_true(before(c("socks", "undershorts", "pants"), "shoes"))
  expect_true(before("shirt", "tie"))
})
