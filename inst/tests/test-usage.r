context("Rd - usage")

test_that("usage captured from formals", {
  out <- test_parse("
    #' Title.
    a <- function(a=1) {}")
  expect_equal(format(out$usage), "a(a\u{A0}=\u{A0}1)")
})

test_that("usage correct for modification functions", {
  out <- test_parse("
    #' Title.
    `foo<-` <- function(a=1) {}")
  
  expect_equal(format(out$usage), "foo(a\u{A0}=\u{A0}1) <- value")
})

test_that("usage correct for functions with no arguments", {
  out <- test_parse("
      #' Function without parameters
      f <- function() 1")
  
  expect_equal(format(out$usage), "f()")
})


test_that("@usage overrides default", {
  out <- test_parse("
    #' @usage a(a=2)
    a <- function(a=1) {}")
    expect_equal(format(out$usage), "a(a=2)")
})

test_that("@usage overrides default for @docType data", {
  out <- test_parse("
    #' Title.
    #'
    #' @name abc
    #' @docType data
    #' @usage data(abc)
    NULL")

  expect_equal(format(out$usage), "data(abc)")
})

test_that("quoted topics have usage statements", {
  out <- test_parse("
    #' Title.
    \"f\" <- function(a = 1, b = 2, c = a + b) {}")
  
  expect_equal(format(out$usage), 
    "f(a\u{A0}=\u{A0}1, b\u{A0}=\u{A0}2, c\u{A0}=\u{A0}a\u{A0}+\u{A0}b)")
  
})

# Output ---------------------------------------------------------------------

test_that("% is escaped in usage", {
  out <- test_parse("
    #' Title.
    a <- function(a='%') {}")
  expect_equal(format(out$usage), "a(a\u{A0}=\u{A0}\"\\%\")")
})

test_that("long usages protected from incorrect breakage", {
  out <- test_parse("
      #' Function long usage
      f <- function(a = '                             a', 
                    b = '                             b', 
                    c = '                             c', 
                    d = '                             ') 1")
  
  usage <- format(out$usage)
  expect_equal(str_count(usage, "\n"), 6)
})
