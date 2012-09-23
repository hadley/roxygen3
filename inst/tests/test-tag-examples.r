context("Tag: @example & @examples")

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

test_that("% in @example are escaped", {
  out <- test_process("
    #' Example
    #' @name a
    #' @example Rd-example-4.R
    NULL")

  examples <- writeRd(tag(out, "examples"))
  expect_match(format(examples), fixed("x \\%*\\% y"))
})

test_that("\\ in @examples are escaped", {
  out <- test_process("
    #' Example
    #' @name a
    #' @examples
    #' '\\'
    NULL")
  examples <- writeRd(tag(out, "examples"))
  expect_match(format(examples), fixed("'\\\\'"))
})

test_that("\\dontrun not unescaped", {
  out <- test_process("
    #' Escaping
    #' @example Rd-example-escapes.r
    #' @examples
    #' \\dontrun{a <- 1}
    #' \\donttest{b <- 2}
    #' \\dontshow{c <- 3}
    #' \\testonly{d <- 4}
    NULL")
  ex <- tag_value(out, "examples")

  expect_false(any(str_detect(ex, fixed("\\\\"))))
})
