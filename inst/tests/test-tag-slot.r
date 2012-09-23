context("Tag: @slot/@autoSlots")

setClass('A')
setClass('B', contains = "A")
setClass('C', contains = "B")

test_that("autoslots documents all slots", {
  out <- test_process("
    #' @autoSlots
    setClass('B', representation(a = 'A', b = 'character', c = 'list'))")
  slots <- tag_value(out, "slot")

  expect_equal(length(slots), 3)
  expect_equal(names(slots), c("a", "b", "c"))
})

test_that("slot only documents specified slots", {
  out <- test_process("
    #' @slot a
    #' @slot b
    setClass('B', representation(a = 'A', b = 'character', c = 'list'))")
  slots <- tag_value(out, "slot")

  expect_equal(length(slots), 2)
  expect_equal(names(slots), c("a", "b"))
})

test_that("slot description overrides default", {
  out <- test_process("
    #' @slot a description a
    #' @slot b
    setClass('B', representation(a = 'A', b = 'character', c = 'list'))")
  slots <- tag_value(out, "slot")

  expect_equal(slots[["a"]], "description a")
  expect_match(slots[["b"]], "An object of class")
})
