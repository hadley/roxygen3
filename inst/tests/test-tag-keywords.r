context("Tag: @keywords")

test_that("keywords split into pieces", {
  out <- test_process("
    #' @keywords a b
    a <- 1")

  expect_equal(tag_value(out, "keywords"), c("a", "b"))
})

test_that("s3 methods and generics default to methods keyword", {
  out1 <- test_process("
    #' My function
    f <- function(x) UseMethod('f')
    ")
  out2 <- test_process("
    #' My function
    c.myclass <- function(x) 1
    ")

  expect_equal(tag_value(out1, "keywords"), "methods")
  expect_equal(tag_value(out2, "keywords"), "methods")
})

test_that("s4 methods and generics default to methods keyword", {
  out1 <- test_process("
    #' My function
    setGeneric('f', function(x) standardGeneric('f'))
    ")
  out2 <- test_process("
    setClass('a')
    #' My function
    setMethod('show', 'a', function(object) object)
    ")

  expect_equal(tag_value(out1, "keywords"), "methods")
  expect_equal(tag_value(out2, "keywords"), "methods")
})

test_that("s4 classes default classes keyword", {
  out <- test_process("
    #' My class
    setClass('a')
    ")

  expect_equal(tag_value(out, "keywords"), "classes")
})
