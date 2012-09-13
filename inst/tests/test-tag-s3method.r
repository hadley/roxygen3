context("Tag: s3method")

test_that("s3method completes as needed", {
  out1 <- test_process("
    #' @s3method
    print.x <- function() {}
  ")
  out2 <- test_process("
    #' @s3method print
    print.x <- function() {}
  ")
  out3 <- test_process("
    #' @s3method print x
    print.x <- function() {}
  ")
  expected <- cbind(generic = "print", class = "x")
  expect_equal(tag_value(out1, "s3method"), expected)
  expect_equal(tag_value(out2, "s3method"), expected)
  expect_equal(tag_value(out3, "s3method"), expected)

})

test_that("s3method completes as needed for compound object", {
  out1 <- test_process("
    #' @s3method
    print.data.frame <- function() {}
  ")
  out2 <- test_process("
    #' @s3method print
    print.data.frame <- function() {}
  ")
  out3 <- test_process("
    #' @s3method print data.frame
    print.data.frame <- function() {}
  ")
  expected <- cbind(generic = "print", class = "data.frame")
  expect_equal(tag_value(out1, "s3method"), expected)
  expect_equal(tag_value(out2, "s3method"), expected)
  expect_equal(tag_value(out3, "s3method"), expected)

})
