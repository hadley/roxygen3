context("Name")

test_that("name captured from assignment", {
  out <- test_parse("
    #' Title.
    a <- function() {} ")
  
  expect_equal(out$name, "a")
  expect_equal(out$alias, "a")
  expect_equal(out$title, "Title.")
})

test_that("name also captured from assignment by =", {
  out <- test_parse("
    #' Title.
    a = function() {} ")
  
  expect_equal(out$name, "a")
  expect_equal(out$alias, "a")
  expect_equal(out$title, "Title.")
})

# test_that("names escaped, not quoted", {
#   out <- test_parse("
#     #' Title
#     '%a%' <- function(x, y) x + y")
#   expect_equal(format(out$name), "\\name{\\%a\\%}\n")
# })

test_that("filename doesn't contain invalid characters", {
  out <- test_parse("
    #' Title.
    #' @name a<-
    NULL
    
    #' Title.
    #' @name a[]
    NULL")
  expect_equal(names(out), c("a-set.Rd", "a-sub.Rd"))
})

test_that("quoted names captured from assignment", {
  out <- test_parse("
    #' Title.
    \"myfunction\" <- function(...) {}")
  
  expect_equal(out$name, "myfunction")
  expect_equal(out$alias, "myfunction")
  
  out <- test_parse("
    #' Title.
    `myfunction` <- function(...) {}")
  expect_equal(out$name, "myfunction")
  expect_equal(out$alias, "myfunction")
  
  out <- test_parse("
    #' Title.
    \"my function\" <- function(...) {}")
  
  expect_equal(out$name, "my function")
  expect_equal(out$alias, "my function")
})

test_that("@name overides default", {
  out <- test_parse("
    #' @name b
    a <- function() {}")
    
    expect_equal(out$name, "b")
    expect_equal(out$alias, "b")
})
