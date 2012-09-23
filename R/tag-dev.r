#' @@dev: flag a function as for developers only.
#'
#' \code{@@dev} flags a function as use primarily for developers building on
#' your package, rather than users. It adds \code{[DEV]} to the title and
#' a brief message to the description.
#'
#' @tagUsage @@dev
setClass("DevTag", contains = "Tag")

setMethod("process", "DevTag", function(input, block) {
  desc <- "This function is aimed primarily at developers extending this package. You shouldn't need to use it for everyday operation of the package."

  title <- tag(block, "title")
  title@text <- str_c("[DEV] ", value(title))
  tag(block, "title") <- title

  tag(block, "description") <- prefix(desc)
  block
})

setMethod("getPrereqs", "DevTag", function(tag) {
  c("IntroTag", "TitleTag", "DetailsTag")
})
