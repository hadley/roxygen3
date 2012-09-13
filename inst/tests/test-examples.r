context("Examples")

test_that("@example loads from specified files", {
  out <- test_process("
    #' @name a
    #' @example Rd-example-1.R
    #' @example Rd-example-2.R
    NULL")

  expect_match(tag_value(out, "examples"), fixed("example <- 'example1'"),
    all = FALSE)
  expect_match(tag_value(out, "examples"), fixed("example <- 'example2'"),
    all = FALSE)
})

test_that("@examples captures examples", {
  out <- test_process("
    #' @name a
    #' @examples a <- 2
    NULL")

  expect_match(tag_value(out, "examples"), fixed("a <- 2"), all = FALSE)
})

test_that("@examples and @example combine", {
  out <- test_process("
    #' @name a
    #' @example Rd-example-1.R
    #' @examples a <- 2
    NULL")

  expect_match(tag_value(out, "examples"), fixed("example <- 'example1'"),
    all = FALSE)
  expect_match(tag_value(out, "examples"), fixed("a <- 2"), all = FALSE)
})

test_that("@example does not introduce extra empty lines", {
  out <- test_process("
    #' @name a
    #' @example Rd-example-3.R
    NULL")

  expect_identical(length(tag_value(out, "examples")), 2L)
})

test_that("indentation in examples preserved", {
  out <- test_process("
    #' @name a
    #' @examples a <-
    #'     2
    NULL")

  expect_match(tag_value(out, "examples"), fixed("a <-\n    2"), all = FALSE)
})

test_that("% in @example escaped", {
  out <- test_process("
    #' Example
    #' @name a
    #' @example Rd-example-4.R
    NULL")

  examples <- writeRd(tag(out, "examples"))
  expect_match(format(examples), fixed("x \\%*\\% y"))
})
