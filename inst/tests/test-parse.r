context("Parse")

test_that("empty file gives empty list", {
  out <- block_parse("")
  expect_identical(out, list())
})

test_that("NULL gives empty list", {
  out <- block_parse("NULL")
  expect_identical(out, list())
})

test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- block_parse("
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)")
    
    expect_equal(out$name, "foo")
})

test_that("deleted objects not documented", {
  out <- roc_process(roc, parse.files("Rd-closure.R"), base_path = ".")
  expect_equal(names(out), "f2.Rd")
})

test_that("@noRd inhibits documentation", {
  out <- block_parse("
    #' Would be title
    #' @title Overridden title
    #' @name a
    #' @noRd
    NULL")
  
  expect_equal(length(out), 0)
})

