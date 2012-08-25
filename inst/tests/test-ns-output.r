context("Namespace: output")


test_that("export escapes quotes name if needed", {
  out <- block_out("#' @export\n'a<-' <- function(){}")
  expect_equal(out$ns_write$NAMESPACE, list('export("a<-")'))
})


test_that("export method escapes if needed", {
  out <- block_out("
    setGeneric('x<-', function(x, value) standardGeneric('x<-'))
    #' @export\n
    setMethod('x<-', 'a', function(x, value) value)")
  expect_equal(out$ns_write$NAMESPACE, list('exportMethods("x<-")'))
})

