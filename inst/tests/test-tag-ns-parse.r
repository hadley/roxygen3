context("Tag: Namespace: parsing")

test_that("export detects object name", {
  out <- test_process("#' @export\na <- function(){}")
  expect_equal(tag_value(out, "export"), "a")
})

test_that("export parameter overrides default", {
  out <- test_process("#' @export b\na <- function(){}")
  expect_equal(tag_value(out, "export"), "b")
})

test_that("export detects S4 class", {
  out <- test_process("#' @export\nsetClass('a')")
  expect_equal(tag_value(out, "exportClass"), "a")
})

test_that("exportClass overrides default class name", {
  out <- test_process("#' @exportClass b\nsetClass('a')")
  expect_equal(tag_value(out, "exportClass"), "b")
})

test_that("export detects method name", {
  out <- test_process("
    setClass('a')
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(tag_value(out, "exportMethods"), "max")
  expect_equal(tag_value(out, "export"), character())
})

test_that("exportMethod overrides default method name", {
  out <- test_process("
    setClass('a')
    #' @exportMethods c
    setMethod('max', 'a', function(x, ...) x[1])")
    expect_equal(tag_value(out, "exportMethods"), "c")
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

  expect_equal(tag_value(out, "exportPattern"), "test")
  expect_equivalent(tag_value(out, "s3method"), cbind("test", "test"))
  expect_equal(tag_value(out, "import"), "test")
  expect_equal(tag_value(out, "importFrom"), c("test1" = "test", test2 = "test"))
  expect_equal(tag_value(out, "importClassesFrom"), c("test", "test1", "test2"))
  expect_equal(tag_value(out, "importMethodsFrom"), c("test", "test1", "test2"))
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
  expect_equal(tag_value(out1, "s3method"), expected)
  expect_equal(tag_value(out2, "s3method"), expected)
  expect_equal(tag_value(out3, "s3method"), expected)

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
  expect_equal(tag_value(out1, "s3method"), expected)
  expect_equal(tag_value(out2, "s3method"), expected)
  expect_equal(tag_value(out3, "s3method"), expected)

})
