context("Namespace: parsing")

test_that("export detects object name", {
  out <- test_parse("#' @export\na <- function(){}")
  expect_equal(out$export, "a")
})

test_that("export parameter overrides default", {
  out <- test_parse("#' @export b\na <- function(){}")
  expect_equal(out$export, "b")
})

test_that("export detects S4 class", {
  out <- test_parse("#' @export\nsetClass('a')")
  expect_equal(out$exportClass, "a")
})

test_that("exportClass overrides default class name", {
  out <- test_parse("#' @exportClass b\nsetClass('a')")
  expect_equal(out$exportClass, "b")
})

test_that("export detects method name", {
  out <- test_parse("
    setClass('a')
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out$exportMethods, "max")
  expect_equal(out[["export"]], NULL) # damn you partial name matching
})

test_that("exportMethod overrides default method name", {
  out <- test_parse("
    setClass('a')
    #' @exportMethods c
    setMethod('max', 'a', function(x, ...) x[1])")
    expect_equal(out$exportMethods, "c")
})
# 
test_that("other namespace tags produce correct output", {
  out <- test_parse("
    #' @exportPattern test
    #' @S3method test test
    #' @import test
    #' @importFrom test test1 test2 
    #' @importClassesFrom test test1 test2
    #' @importMethodsFrom test test1 test2
    #' @name dummy
    NULL")
    
  expect_equal(out$exportPattern, "test")
  expect_equal(out$S3method, c("test", "test"))
  expect_equal(out$import, "test")
  expect_equal(out$importFrom, c("test1" = "test", test2 = "test"))
  expect_equal(out$importClassesFrom, c("test", "test1", "test2"))
  expect_equal(out$importMethodsFrom, c("test", "test1", "test2"))
})

test_that("S3method completes as needed", {
  out1 <- test_parse("
    #' @S3method
    print.x <- function() {}
  ")
  out2 <- test_parse("
    #' @S3method print
    print.x <- function() {}
  ")
  out3 <- test_parse("
    #' @S3method print x
    print.x <- function() {}
  ")
  expect_equal(out1$S3method, c("print", "x"))
  expect_equal(out2$S3method, c("print", "x"))
  expect_equal(out3$S3method, c("print", "x"))
  
})

test_that("S3method completes as needed for compound object", {
  out1 <- test_parse("
    #' @S3method
    print.data.frame <- function() {}
  ")
  out2 <- test_parse("
    #' @S3method print
    print.data.frame <- function() {}
  ")
  out3 <- test_parse("
    #' @S3method print data.frame
    print.data.frame <- function() {}
  ")
  expect_equal(out1$S3method, c("print", "data.frame"))
  expect_equal(out2$S3method, c("print", "data.frame"))
  expect_equal(out3$S3method, c("print", "data.frame"))
  
})