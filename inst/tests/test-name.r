context("Name")

test_that("name captured from assignment", {
  out <- test_process("
    #' Title.
    a <- function() {} ")
  
  expect_equal(out$name@text, "a")
  expect_equal(out$alias@text, "a")
  expect_equal(out$title@text, "Title.")
})

test_that("name also captured from assignment by =", {
  out <- test_process("
    #' Title.
    a = function() {} ")
  
  expect_equal(out$name@text, "a")
  expect_equal(out$alias@text, "a")
  expect_equal(out$title@text, "Title.")
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
  
  expect_equal(out$name@text, "myfunction")
  expect_equal(out$alias@text, "myfunction")
  
  out <- test_process("
    #' Title.
    `myfunction` <- function(...) {}")
  expect_equal(out$name@text, "myfunction")
  expect_equal(out$alias@text, "myfunction")
  
  out <- test_process("
    #' Title.
    \"my function\" <- function(...) {}")
  
  expect_equal(out$name@text, "my function")
  expect_equal(out$alias@text, "my function")
})

test_that("@name overides default", {
  out <- test_process("
    #' @name b
    a <- function() {}")
    
    expect_equal(out$name@text, "b")
    expect_equal(out$alias@text, "b")
})

# S4 -------------------------------------------------------------------------

test_that("S4 class names have -class suffix", {
  out <- test_process("
    #' Title
    setClass('a')")
    
  expect_equal(out$name@text, "a-class")
})

test_that("S4 method names contain signature and have -method suffix", {
  out <- test_process("
    setGeneric('a', function(a) standardGeneric('a'))
    #' Title
    setMethod('a', 'numeric', function(a) 1)")
    
  expect_equal(out$name@text, "a,numeric-method")
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
    
  expect_equal(out1$name@text, "a,MISSING-method")
  expect_equal(out2$name@text, "a,ANY-method")
})



