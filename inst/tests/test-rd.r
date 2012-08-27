context("Rd")

# Names and filenames --------------------------------------------------------


test_that("keywords and aliases split into pieces", {
  out <- block_parse("
    #' @keywords a b
    #' @aliases a b
    #' @name a
    NULL")
    
  expect_match(out$keyword, fixed("a"), all = FALSE)
  expect_match(out$keyword, fixed("b"), all = FALSE)
  expect_match(out$alias, fixed("a"), all = FALSE)
  expect_match(out$alias, fixed("b"), all = FALSE)
})

test_that("generic keys produce expected output", {
  out <- block_parse("
    #' @references test
    #' @note test
    #' @author test
    #' @seealso test
    #' @concept test
    #' @encoding test
    #' @name a
    NULL")
  expect_equal(out$references, "test")
  expect_equal(out$note, "test")
  expect_equal(out$seealso, "test")
  expect_equal(out$concept, "test")
  expect_equal(out$encoding, "test")
  expect_equal(out$author, "test")
})

