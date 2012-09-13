context("Tag: Usage S4")

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
