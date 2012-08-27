context("rdname")

test_that("All rocs with the same @rdfile are merged into one", {
  out <- test_output("
    #' Function a
    #' @param a a
    #' @rdname shared
    a <- function(a) {}
    
    #' Function b
    #' @param b a
    #' @rdname shared
    b <- function(b) {}
    ")
  
  shared <- out$rd_out$`man/shared.Rd`
  expect_equal(length(shared$alias$values), 2)
  expect_equal(length(shared$arguments$values), 2)
  expect_equal(format(shared$title), "\\title{Function a}\n")
  
  usage <- format(shared$usage)
  expect_match(usage, fixed("a(a)"), all = FALSE)
  expect_match(usage, fixed("b(b)"), all = FALSE)  
})