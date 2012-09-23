context("Tag: @param")

test_that("@param documents arguments", {
  out <- test_process("
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")

  args <- tag_value(out, "param")
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})
