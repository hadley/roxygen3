context("Tag: @inheritParams")

test_that("multiple @inheritParams tags gathers all params", {
  out <- test_process("
    #' A.
    #' @param x X
    a <- function(x) {}

    #' B
    #' @param y Y
    b <- function(y) {}

    #' D
    #' @inheritParams a
    #' @inheritParams b
    d <- function(x, y) {}")

  args <- tag_value(out, "param")
  expect_equal(length(args), 2)

  expect_equal(args[["x"]], "X")
  expect_equal(args[["y"]], "Y")
})

test_that("@inheritParams inherits from functions in other packages", {
  out <- test_process("
    #' My mean
    #'
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")

  args <- tag_value(out, "param")
  expect_equal(length(args), 2)
  expect_equal(sort(names(args)), c("trim", "x"))
})


test_that("@inheritParams inherits from functions in other packages", {
  out <- test_process("
    #' Function a.
    #'
    #' @param a parameter a
    a <- function(a = 1) {}

    #' Function b
    #'
    #' @inheritParams a
    #' @param b parameter b
    b <- function(a = 1, b = 2) {}")

  args <- tag_value(out, "param")
  expect_equal(length(args), 2)
  expect_equal(sort(names(args)), c("a", "b"))
})

test_that("@inheritParams only add missing params", {
  out <- test_process("
    #' Function a.
    #'
    #' @param a param 1
    a <- function(a = 1) {}

    #' Function b
    #'
    #' @inheritParams a
    #' @param a param 2
    b <- function(a = 1) {}")

  args <- tag_value(out, "param")
  expect_equal(length(args), 1)
  expect_equal(args[[1]], "param 2")
})
