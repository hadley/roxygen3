#' Override default description.
#'
#' The topic title. By default this is taken from the second paragraph of the
#' roxygen block. See \code{\linkS4class{IntroTag}} for more details.
#'
#' @tagUsage @@description Text goes here.
setClass("DescriptionTag", contains = "Tag")

setMethod("writeRd", "DescriptionTag", function(object) {
  RdCommand("description", object@text)
})
