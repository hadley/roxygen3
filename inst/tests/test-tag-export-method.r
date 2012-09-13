context("Tag: @exportMethod")

test_that("export method escapes if needed", {
  out <- test_ns("
    setGeneric('x<-', function(x, value) standardGeneric('x<-'))
    #' @export\n
    setMethod('x<-', 'a', function(x, value) value)")
  expect_equal(out, 'exportMethods("x<-")')
})

test_that("exportMethod overrides default method name", {
  out <- test_process("
    setClass('a')
    #' @exportMethods c
    setMethod('max', 'a', function(x, ...) x[1])")
    expect_equal(tag_value(out, "exportMethods"), "c")
})
