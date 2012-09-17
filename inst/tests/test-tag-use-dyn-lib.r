context("Tag: @useDynLib")

test_that("empty tag inspects function to determine calls", {
  out1 <- test_process("
    #' @useDynLib
    f <- function() .Call('test')")
  out2 <- test_process("
    #' @useDynLib
    f <- function() .C('test')")
  out3 <- test_process("
    #' @useDynLib
    f <- function() .Fortran('test')")
  out4 <- test_process("
    #' @useDynLib
    f <- function() .External('test')")

  expect_equal(tag_value(out1, "useDynLib"), "temp,test")
  expect_equal(tag_value(out2, "useDynLib"), "temp,test")
  expect_equal(tag_value(out3, "useDynLib"), "temp,test")
  expect_equal(tag_value(out4, "useDynLib"), "temp,test")
})

test_that("manual dyn lib directives are preserved as is", {
  out <- test_process("
    #' @useDynLib mypackage, myfunction, blah = TRUE
    f <- function() .Call('test')")

  expect_equal(tag_value(out, "useDynLib"),
    "mypackage, myfunction, blah = TRUE")

})

test_that("namespace directive computed correctly", {
  out <- test_process("
    #' @useDynLib mypackage
    f <- function() .Call('test')")

  expect_equal(writeNamespace(out)[[1]], "useDynLib(mypackage)")
})

test_that("old form still works" ,{
  out <- test_process("
    #' @useDynLib mypackage a b
    f <- function() .Call('test')")

  dynlib <- tag_value(out, "useDynLib")
  expect_equal(dynlib[1], "mypackage,a")
  expect_equal(dynlib[2], "mypackage,b")
})
