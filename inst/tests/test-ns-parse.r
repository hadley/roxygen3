context("Namespace: parsing")

test_that("export detects object name", {
  out <- test_process("#' @export\na <- function(){}")
  expect_equal(out$export@text, "a")
})

test_that("export parameter overrides default", {
  out <- test_process("#' @export b\na <- function(){}")
  expect_equal(out$export@text, "b")
})

test_that("export detects S4 class", {
  out <- test_process("#' @export\nsetClass('a')")
  expect_equal(out$exportClass@text, "a")
})

test_that("exportClass overrides default class name", {
  out <- test_process("#' @exportClass b\nsetClass('a')")
  expect_equal(out$exportClass@text, "b")
})

test_that("export detects method name", {
  out <- test_process("
    setClass('a')
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out$exportMethods@text, "max")
  # damn you partial name matching
  expect_equal(out[["export"]]@text, character()) 
})

test_that("exportMethod overrides default method name", {
  out <- test_process("
    setClass('a')
    #' @exportMethods c
    setMethod('max', 'a', function(x, ...) x[1])")
    expect_equal(out$exportMethods@text, "c")
})
# 
test_that("other namespace tags produce correct output", {
  out <- test_process("
    #' @exportPattern test
    #' @s3method test test
    #' @import test
    #' @importFrom test test1 test2 
    #' @importClassesFrom test test1 test2
    #' @importMethodsFrom test test1 test2
    #' @name dummy
    NULL")
    
  expect_equal(out$exportPattern@text, "test")
  expect_equivalent(out$s3method@methods, cbind("test", "test"))
  expect_equal(out$import@text, "test")
  expect_equal(out$importFrom@imports, c("test1" = "test", test2 = "test"))
  expect_equal(out$importClassesFrom@text, c("test", "test1", "test2"))
  expect_equal(out$importMethodsFrom@text, c("test", "test1", "test2"))
})

test_that("S3method completes as needed", {
  out1 <- test_process("
    #' @s3method
    print.x <- function() {}
  ")
  out2 <- test_process("
    #' @s3method print
    print.x <- function() {}
  ")
  out3 <- test_process("
    #' @s3method print x
    print.x <- function() {}
  ")
  expected <- cbind(generic = "print", class = "x")
  expect_equal(out1$s3method@methods, expected)
  expect_equal(out2$s3method@methods, expected)
  expect_equal(out3$s3method@methods, expected)
  
})

test_that("S3method completes as needed for compound object", {
  out1 <- test_process("
    #' @s3method
    print.data.frame <- function() {}
  ")
  out2 <- test_process("
    #' @s3method print
    print.data.frame <- function() {}
  ")
  out3 <- test_process("
    #' @s3method print data.frame
    print.data.frame <- function() {}
  ")
  expected <- cbind(generic = "print", class = "data.frame")
  expect_equal(out1$s3method@methods, expected)
  expect_equal(out2$s3method@methods, expected)
  expect_equal(out3$s3method@methods, expected)
  
})