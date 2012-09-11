context("Params")

test_that("@param documents arguments", {
  out <- test_process("
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")
    
  args <- out$param@arguments
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

test_that("multiple @inheritParam tags gathers all params", {
  out <- test_process("  
    #' A.
    #' @param x X
    a <- function(x) {}

    #' B
    #' @param y Y
    b <- function(y) {}

    #' C
    #' @inheritParams a
    #' @inheritParams b
    c <- function(x, y) {}")

  expect_equal(length(out$param@arguments), 2)
  
  expect_equal(out$param@arguments[["x"]], "X")
  expect_equal(out$param@arguments[["y"]], "Y")  
})

test_that("@inheritParam inherits from functions in other packages", {
  out <- test_process("
    #' My mean
    #' 
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")
  params <- out$param@arguments
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})


test_that("@inheritParam inherits from functions in other packages", {
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

  expect_equal(length(out$param@arguments), 2)
  expect_equal(sort(names(out$param@arguments)), c("a", "b"))
})

test_that("@inheritParam only add missing params", {
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

  expect_equal(length(out$param@arguments), 1)
  expect_equal(out$param@arguments[[1]], "param 2")
})
