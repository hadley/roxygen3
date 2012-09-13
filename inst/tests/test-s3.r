context("S3 methods/generics")

test_that("primitive generics detected", {
  expect_true(is_s3_generic("["))
  expect_true(is_s3_method("[.data.frame"))

  expect_true(is_s3_generic("mean"))
  expect_true(is_s3_method("[.default"))

  expect_true(is_s3_generic("c"))
  expect_true(is_s3_method("c.Date"))
})

test_that("user defined generics & methods detected", {
  my_method <- function(x) UseMethod("mymethod")
  my_method.character <- function(x) x

  expect_true(is_s3_generic("my_method"))
  expect_true(is_s3_method("my_method.character"))
})
