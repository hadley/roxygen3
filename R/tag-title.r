#' Override default topic title.
#'
#' The topic title. By default this is taken from the first paragraph of the
#' roxygen block. See \code{\link{tag_intro}} for more details.
#'
#' @usageTag @@title Topic title
setClass("TitleTag", contains = "Tag")
setMethod("writeRd", "TitleTag", function(object) {
  new_command("title", object@text)
})
