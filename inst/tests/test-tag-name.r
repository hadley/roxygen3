context("Tag: @name")

test_that("name captured from assignment", {
  out <- test_process("
    #' Title.
    a <- function() {} ")

  expect_equal(tag_value(out, "name"), "a")
  expect_equal(tag_value(out, "aliases"), "a")
  expect_equal(tag_value(out, "title"), "Title.")
})

test_that("name also captured from assignment by =", {
  out <- test_process("
    #' Title.
    a = function() {} ")

  expect_equal(tag_value(out, "name"), "a")
  expect_equal(tag_value(out, "aliases"), "a")
  expect_equal(tag_value(out, "title"), "Title.")
})

# test_that("names escaped, not quoted", {
#   out <- test_process("
#     #' Title
#     '%a%' <- function(x, y) x + y")
#   expect_equal(format(out$name), "\\name{\\%a\\%}\n")
# })

test_that("filename doesn't contain invalid characters", {
  out <- test_rd("
    #' Title.
    #' @name a<-
    NULL

    #' Title.
    #' @name a[]
    NULL")
  expect_equal(names(out), c("man/a-set.Rd", "man/a-sub.Rd"))
})

test_that("quoted names captured from assignment", {
  out <- test_process("
    #' Title.
    \"myfunction\" <- function(...) {}")

  expect_equal(tag_value(out, "name"), "myfunction")
  expect_equal(tag_value(out, "aliases"), "myfunction")

  out <- test_process("
    #' Title.
    `myfunction` <- function(...) {}")
  expect_equal(tag_value(out, "name"), "myfunction")
  expect_equal(tag_value(out, "aliases"), "myfunction")

  out <- test_process("
    #' Title.
    \"my function\" <- function(...) {}")

  expect_equal(tag_value(out, "name"), "my function")
  expect_equal(tag_value(out, "aliases"), "my-function")
})

test_that("@name overides default", {
  out <- test_process("
    #' @name b
    a <- function() {}")

    expect_equal(tag_value(out, "name"), "b")
    expect_equal(tag_value(out, "aliases"), "b")
})

# S4 -------------------------------------------------------------------------

test_that("S4 class names have -class suffix", {
  out <- test_process("
    #' Title
    setClass('a')")

  expect_equal(tag_value(out, "name"), "a-class")
})

test_that("S4 method names contain signature and have -method suffix", {
  out <- test_process("
    setGeneric('a', function(a) standardGeneric('a'))
    #' Title
    setMethod('a', 'numeric', function(a) 1)")

  expect_equal(tag_value(out, "name"), "a,numeric-method")
})

test_that("S4 method names contain ANY and MISSING in signature", {
  out1 <- test_process("
    setGeneric('a', function(a) standardGeneric('a'))
    #' Title
    setMethod('a', 'MISSING', function(a) 1)")
  out2 <- test_process("
    setGeneric('a', function(a) standardGeneric('a'))
    #' Title
    setMethod('a', 'ANY', function(a) 1)")

  expect_equal(tag_value(out1, "name"), "a,MISSING-method")
  expect_equal(tag_value(out2, "name"), "a,ANY-method")
})



