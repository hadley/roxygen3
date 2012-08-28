context("Include")

roxy_dir <- function(path, roccers = base_roccers()) {
  rocblocks <- parse_directory(path)
  rocblocks <- roxy_process(rocblocks, roccers)  
  out <- roxy_out(rocblocks, roccers)
  roxy_postproc(out)
}

test_that("included files come earlier", {
  out <- in_dir("include-clothes", roxy_dir("."))
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
