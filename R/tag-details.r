#' @@details: provide (lots of) extra detail about the function
#'
#' The topic title. By default this is taken from the third and subsequent
#' paragraphs of the roxygen block. See \code{\linkS4class{IntroTag}} for more
#' details.
#'
#' @tagUsage @@details Text goes here.
setClass("DetailsTag", contains = "Tag")
setMethod("writeRd", "DetailsTag", function(object) {
  RdCommand("details", object@text)
})
