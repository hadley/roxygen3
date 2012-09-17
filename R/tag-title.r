#' Override default topic title.
#'
#' The topic title. By default this is taken from the first paragraph of the
#' roxygen block. See \code{\linkS4class{IntroTag}} for more details.
#'
#' @usageTag @@title Topic title
setClass("TitleTag", contains = "Tag")
setMethod("writeRd", "TitleTag", function(object) {
  RdCommand("title", object@text)
})
