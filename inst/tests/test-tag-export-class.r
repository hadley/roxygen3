context("Tag: @exportClass")

test_that("exportClass overrides default class name", {
  out <- test_process("#' @exportClass b\nsetClass('a')")
  expect_equal(tag_value(out, "exportClass"), "b")
})
