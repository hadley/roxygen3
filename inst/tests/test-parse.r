context("Parse")

test_that("empty file gives empty list", {
  out <- parse_block("")
  expect_identical(out, list())
})

test_that("NULL gives empty list", {
  out <- parse_block("NULL")
  expect_identical(out, list())
})

test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- test_process("
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)")
    
    expect_equal(out$name, "foo")
})

test_that("deleted objects not documented", {
  rocblocks <- roxy_process(parse_file("rd-closure.r"))
  out <- roxy_out(rocblocks)
  
  expect_equal(length(out$rd_write), 1)
})

test_that("@noRd inhibits rd, but not namespace output", {
  rocblock <- test_process("
    #' Would be title
    #' @title Overridden title
    #' @name a
    #' @noRd
    NULL")
  out <- roxy_out(list(rocblock))
  expect_equal(length(out$rd_write), 0)
})

