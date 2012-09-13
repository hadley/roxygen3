context("Parse")

test_that("empty file gives empty list", {
  out <- parse_block("")
  expect_identical(out@blocks, list())
})

test_that("NULL gives single code block", {
  out <- parse_block("NULL")
  expect_identical(length(out@blocks), 1L)
})

test_that("single comment gives empty list", {
  out <- parse_block("# comment")
  expect_identical(out@blocks, list())
})

test_that("commented out roxygen block gives empty list", {
  out <- parse_block("# #' comment")
  expect_identical(out@blocks, list())
})

test_that("empty roxygen comment doesn't give error", {
  out <- parse_block("#'\nNULL")
  expect_identical(length(out@blocks), 1L)
})

test_that("`$` not to be parsed as assignee in foo$bar(a = 1)", {
  out <- test_process("
    #' foo object
    foo <- list(bar = function(a) a)
    foo$bar(a = 1)")

  expect_equal(tag_value(out, "name"), NULL)
})

test_that("@noRd inhibits rd, but not namespace output", {
  out <- test_rd("
    #' Would be title
    #' @title Overridden title
    #' @name a
    #' @noRd
    NULL")
  expect_equal(length(out), 0)
})

