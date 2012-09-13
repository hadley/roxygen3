context("Title, description, details")

test_that("title and description taken from first line if only one", {
  out <- test_process("
    #' description
    #' @name a
    NULL")
  expect_equal(tag_value(out, "description"), "description")
  expect_equal(tag_value(out, "title"), "description")
})

test_that("title, description and details extracted correctly", {
  out <- test_process("
    #' title
    #'
    #' description
    #'
    #' details
    #' @name a
    NULL")
  expect_equal(tag_value(out, "description"), "description")
  expect_equal(tag_value(out, "details"), "details")
})

test_that("title taken from first paragraph", {
  out <- test_process("
    #' Description with sentence.
    #'
    #' That continueth.
    #' @name a
    NULL")
  expect_equal(tag_value(out, "title"), "Description with sentence.")
  expect_equal(tag_value(out, "description"),
    "That continueth.")
})

test_that("@title overrides default title", {
  out <- test_process("
    #' Would be title
    #' @title Overridden title
    #' @name a
    NULL")
  expect_equal(tag_value(out, "title"), "Overridden title")
  expect_equal(tag_value(out, "description"), "Would be title")
})

test_that("docs parsed correctly if no blank text", {
  out <- test_process("
    #' @title My title
    #' @description My description
    #' @param x value
    a <- function(x) {}")

  expect_equal(tag_value(out, "title"), "My title")
  expect_equal(tag_value(out, "description"), "My description")
})

test_that("question mark ends sentence", {
  out <- test_process("
    #' Is a number odd?
    is.odd <- function(a) {}")
  expect_equal(tag_value(out, "title"), "Is a number odd?")

})

test_that("no ending punctuation does not produce ellipsis", {
  out <- test_process("
    #' Whether a number is odd
    is.odd <- function(a) {}")
  expect_equal(tag_value(out, "title"), "Whether a number is odd")
})
