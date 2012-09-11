context("Simple tags")

test_that("keywords and aliases split into pieces", {
  out <- test_process("
    #' @keywords a b
    #' @aliases a b
    #' @name a
    NULL")
    
  expect_match(out$keyword@text, fixed("a"), all = FALSE)
  expect_match(out$keyword@text, fixed("b"), all = FALSE)
  expect_match(out$alias@text, fixed("a"), all = FALSE)
  expect_match(out$alias@text, fixed("b"), all = FALSE)
})

test_that("generic keys produce expected output", {
  out <- test_process("
    #' @references test
    #' @note test
    #' @author test
    #' @seealso test
    #' @concept test
    #' @encoding test
    #' @name a
    NULL")
  expect_equal(out$references@text, "test")
  expect_equal(out$note@text, "test")
  expect_equal(out$seealso@text, "test")
  expect_equal(out$concept@text, "test")
  expect_equal(out$encoding@text, "test")
  expect_equal(out$author@text, "test")
})

