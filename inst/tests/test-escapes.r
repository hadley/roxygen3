context("Escapes")

test_that("escape examples leaves plain text unchanged", {
  expect_equal(escape_examples("a"), "a")
  expect_equal(escape_examples("a+-()!@#$%%^&*("), "a+-()!@#$%%^&*(")
})

test_that("escape examples escapes backslashes", {
  expect_equal(escape_examples("\\"), "\\\\")
  expect_equal(escape_examples("\\a"), "\\\\a")
  expect_equal(escape_examples("\\\\"), "\\\\\\\\")
})

test_that("special macros are not escaped", {
  expect_equal(escape_examples("\\donttest"), "\\donttest")
  expect_equal(escape_examples("\\dontrun"), "\\dontrun")
  expect_equal(escape_examples("\\dontshow"), "\\dontshow")
})
