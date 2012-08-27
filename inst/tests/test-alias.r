context("Alias")

# test_that("aliases escaped, not quoted", {
#   out1 <- block_parse("
#     #' @aliases a
#     #' @name %a%
#     NULL")
#   out2 <- block_parse("
#     #' @aliases %a%
#     #' @name a
#     NULL")
#   alias1 <- format(get_tag(out1, "alias"))
#   alias2 <- format(get_tag(out2, "alias"))
#   expect_equal(alias1, c("\\alias{\\%a\\%}\n", "\\alias{a}\n"))
#   expect_equal(alias2, c("\\alias{\\%a\\%}\n", "\\alias{a}\n"))
# })
