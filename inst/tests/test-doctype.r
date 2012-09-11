context("docTypes")

# Package --------------------------------------------------------------------

test_that("@docType package automatically adds package alias when needed", {
  out1 <- test_process("
    #' @name a
    #' @docType package
    NULL")
  out2 <- test_process("
    #' @name a-package
    #' @docType package
    NULL")
  
  expect_equal(sort(out1$aliases@text), sort(c("a", "a-package")))
  expect_equal(out2$aliases@text, c("a-package"))
})


# Data --------------------------------------------------------------------

test_that("@docType data automatically adds sensible defaults", {
  out <- test_process("
    #' Title.
    #'
    #' @docType data
    a <- data.frame(a = 1:10)")
  
  expect_equal(out$usage@text, "a")
  expect_equal(out$keywords@text, "datasets")
  expect_match(out$format@text, "data\\.frame")
})

test_that("@docType data automatically added to data objects", {
  out <- test_process("
    #' Title.
    a <- data.frame(a = 1:10)")
  
  expect_equal(out$docType, "data")
})

# Reference classes ----------------------------------------------------------

test_that("@docType data not automatically added to reference classes", {
  out <- test_process("
    #' Title.
    a <- setRefClass('a')")
  
  expect_equal(out$docType, NULL)  
})