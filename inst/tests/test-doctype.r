context("Rd - docTypes")

# Package --------------------------------------------------------------------

test_that("@docType package automatically adds package alias when needed", {
  out1 <- block_parse("
    #' @name a
    #' @docType package
    NULL")
  out2 <- block_parse("
    #' @name a-package
    #' @docType package
    NULL")
  
  expect_equal(sort(out1$aliases), sort(c("a", "a-package")))
  expect_equal(out2$aliases, c("a-package"))
})


# Data --------------------------------------------------------------------

test_that("@docType data automatically adds sensible defaults", {
  out <- block_parse("
    #' Title.
    #'
    #' @docType data
    a <- data.frame(a = 1:10)")
  
  expect_equal(out$usage, "a")
  expect_equal(out$keywords, "datasets")
  expect_match(out$format, "data\\.frame")
})

test_that("@docType data automatically added to data objects", {
  out <- block_parse("
    #' Title.
    a <- data.frame(a = 1:10)")
  
  expect_equal(out$docType, "data")  
})

# Reference classes ----------------------------------------------------------

test_that("@docType data not automatically added to reference classes", {
  out <- block_parse("
    #' Title.
    a <- setRefClass('a')")
  
  expect_equal(out$docType, NULL)  
})