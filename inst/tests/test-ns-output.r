context("Namespace: output")


test_that("export escapes quotes name if needed", {
  out <- test_ns("#' @export\n'a<-' <- function(){}")
  expect_equal(out, 'export("a<-")')
})


test_that("export method escapes if needed", {
  out <- test_ns("
    setGeneric('x<-', function(x, value) standardGeneric('x<-'))
    #' @export\n
    setMethod('x<-', 'a', function(x, value) value)")
  expect_equal(out, 'exportMethods("x<-")')
})

