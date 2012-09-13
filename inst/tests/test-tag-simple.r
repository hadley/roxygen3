context("Tag: Simple")

test_that("keywords and aliases split into pieces", {
  out <- test_process("
    #' @keywords a b
    #' @aliases a b
    #' @name a
    NULL")

  expect_match(tag_value(out, "keywords"), fixed("a"), all = FALSE)
  expect_match(tag_value(out, "keywords"), fixed("b"), all = FALSE)
  expect_match(tag_value(out, "aliases"), fixed("a"), all = FALSE)
  expect_match(tag_value(out, "aliases"), fixed("b"), all = FALSE)
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
  expect_equal(tag_value(out, "references"), "test")
  expect_equal(tag_value(out, "note"), "test")
  expect_equal(tag_value(out, "seealso"), "test")
  expect_equal(tag_value(out, "concept"), "test")
  expect_equal(tag_value(out, "encoding"), "test")
  expect_equal(tag_value(out, "author"), "test")
})

