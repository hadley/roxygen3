context("Tag: @usage")
library(stringr)

test_that("usage captured from formals", {
  out <- test_process("
    #' Title.
    a <- function(a=1) {}")
  expect_equal(format(tag(out, "usage")), "a(a\u{A0}=\u{A0}1)")
})

test_that("usage correct for modification functions", {
  out <- test_process("
    #' Title.
    `foo<-` <- function(a=1) {}")

  expect_equal(format(tag(out, "usage")), "foo(a\u{A0}=\u{A0}1) <- value")
})

test_that("usage correct for functions with no arguments", {
  out <- test_process("
      #' Function without parameters
      f <- function() 1")

  expect_equal(format(tag(out, "usage")), "f()")
})

test_that("@usage overrides default", {
  out <- test_process("
    #' @usage a(a=2)
    a <- function(a=1) {}")
    expect_equal(format(tag(out, "usage")), "a(a=2)")
})

test_that("@usage overrides default for @docType data", {
  out <- test_process("
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")

  expect_equal(format(tag(out, "usage")), "data(abc)")
})

test_that("quoted topics have usage statements", {
  out <- test_process("
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")

  expect_equal(format(tag(out, "usage")),
    "f(a\u{A0}=\u{A0}1, b\u{A0}=\u{A0}2, c\u{A0}=\u{A0}a\u{A0}+\u{A0}b)")

})

test_that("S3 methods use \\method", {
  out <- test_process("
    foo <- function(x) UseMethod('foo')
    #' Title.
    foo.numeric <- function(x) x")

  expect_equal(format(tag(out, "usage")), "\\method{foo}{numeric}(x)")
})

test_that("S3 replace methods use \\method and <- value", {
  out <- test_process("
    'foo<-' <- function(x) UseMethod('foo<-')
    #' Title.
    'foo<-.numeric' <- function(x) x")

  expect_equal(format(tag(out, "usage")), "\\method{foo}{numeric}(x) <- value")
})

test_that("custom infix operators look right", {
  out <- test_process("
    #' Title.
    '%test%' <- function(a, b) {}")

  expect_equal(format(tag(out, "usage")), "a %test% b")
})

# Output ---------------------------------------------------------------------

test_that("% is escaped in usage", {
  out <- test_process("
    #' Title.
    a <- function(a='%') {}")
  expect_equal(format(tag(out, "usage")), "a(a\u{A0}=\u{A0}\"\\%\")")
})

test_that("\\ is not over escaped in usage", {
  input <- "
    #' Title.
    a <- function(a='\\\"') {}"
  out <- test_process(input)
  expect_equal(format(tag(out, "usage")), "a(a\u{A0}=\u{A0}\"\\\"\")")
})


test_that("long usages protected from incorrect breakage", {
  out <- test_process("
      #' Function long usage
      f <- function(a = '                             a',
                    b = '                             b',
                    c = '                             c',
                    d = '                             ') 1")

  usage <- format(writeRd(tag(out, "usage")))
  expect_equal(str_count(usage, "\n"), 5)
})

test_that("correct usage for S4 generics", {

  out <- test_process("
    #' Test generic.
    setGeneric('test', function(object) {standardGeneric('test')})")

  expect_equal(format(tag(out, "usage")), "test(object)")
})

test_that("correct usage for S4 methods", {

  out <- test_process("
    setGeneric('test', function(x) {standardGeneric('test')})
    #' Test method.
    setMethod('test', signature(x = 'numeric'), function(x = 1) {})
    ")

  expect_equal(
    format(tag(out, "usage")),
    "\\S4method{test}{numeric}(x\u{A0}=\u{A0}1)")
})
