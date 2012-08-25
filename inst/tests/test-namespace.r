context("Namespace: parsing")

test_that("export detects object name", {
  out <- block_parse("#' @export\na <- function(){}")
  expect_equal(out$export, "a")
})

test_that("export parameter overrides default", {
  out <- block_parse("#' @export b\na <- function(){}")
  expect_equal(out$export, "b")
})

test_that("export detects S4 class", {
  out <- block_parse("#' @export\nsetClass('a')")
  expect_equal(out$exportClass, "a")
})

test_that("exportClass overrides default class name", {
  out <- block_parse("#' @exportClass b\nsetClass('a')")
  expect_equal(out$exportClass, "b")
})

test_that("export detects method name", {
  out <- block_parse("
    setClass('a')
    #' @export\n
    setMethod('max', 'a', function(x, ...) x[1])")
  expect_equal(out$exportMethod, "max")  
})

# test_that("export method escapes if needed", {
#   out <- block_parse("
#     setGeneric('x<-', function(x, value) standardGeneric('x<-'))
#     #' @export\n
#     setMethod('x<-', 'a', function(x, value) value)")
#   expect_equal(out, 'exportMethods("x<-")')  
# })
# 
# 
# test_that("exportMethod overrides default method name", {
#   out <- block_parse("
#     #' @exportMethod c
#     setMethod('max', 'a', function(x, ...) x[1])")
#   expect_equal(out, 'exportMethods(c)')  
# })
# 
# test_that("other namespace tags produce correct output", {
#   out <- block_parse("
#     #' @exportPattern test
#     #' @S3method test test
#     #' @import test
#     #' @importFrom test test1 test2 
#     #' @importClassesFrom test test1 test2
#     #' @importMethodsFrom test test1 test2
#     NULL")
# 
#   expect_equal(sort(out), sort(c(
#     "exportPattern(test)", 
#     "S3method(test,test)",
#     "import(test)", 
#     "importFrom(test,test1)", 
#     "importFrom(test,test2)",
#     "importClassesFrom(test,test1)", 
#     "importClassesFrom(test,test2)", 
#     "importMethodsFrom(test,test1)",
#     "importMethodsFrom(test,test2)"
#   )))
# })
# 
# test_that("useDynLib imports only selected functions", {
#   out <- block_parse("
#     #' @useDynLib test
#     #' @useDynLib test a
#     #' @useDynLib test a b
#     NULL")
#   
#     expect_equal(sort(out), sort(
#       c("useDynLib(test)", "useDynLib(test,a)", "useDynLib(test,b)")))
# })