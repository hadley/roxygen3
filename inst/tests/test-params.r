context("Params")

test_that("@param documents arguments", {
  out <- block_parse("
    #' @param a an incipit letter
    #' @param z a terminal letter
    a <- function(a=1, z=2) {}")
    
  args <- out$param
  expect_equivalent(args["a"], "an incipit letter")
  expect_equivalent(args["z"], "a terminal letter")
})

# test_that("multiple @inheritParam tags gathers all params", {
#   out <- roc_process(roc, parse.files("rd-params.r"), base_path = ".")
#   
#   params <- get_tag(out[["c.Rd"]], "arguments")$values
#   expect_equal(length(params), 2)
#   
#   expect_equal(params[["x"]], "X")
#   expect_equal(params[["y"]], "Y")  
# })

test_that("multiple @inheritParam inherits from existing topics", {
  out <- block_parse("
    #' My mean
    #' 
    #' @inheritParams base::mean
    mymean <- function(x, trim) {}")
  params <- out$param
  expect_equal(length(params), 2)
  expect_equal(sort(names(params)), c("trim", "x"))
})
