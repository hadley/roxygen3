context("Tag: @export")

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

test_that("exporting generic exports both function and methods", {
  out <- test_process("
    #' @export
    setGeneric('a', function(x) standardGeneric('a'))")
  expect_equal(tag_value(out, "exportMethods"), "a")
  expect_equal(tag_value(out, "exportMethods"), "a")

})

test_that("export escapes quotes name if needed", {
  out <- test_ns("#' @export\n'a<-' <- function(){}")
  expect_equal(out, 'export("a<-")')
})

test_that("export detects method name", {
  out <- test_process("
    setClass('a')
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(tag_value(out, "exportMethods"), "max")
  expect_equal(tag_value(out, "export"), character())
})

