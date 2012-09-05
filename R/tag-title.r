#' Override default topic title.
#'
#' The topic title. By default this is taken from the first paragraph of the
#' roxygen block. See \code{\link{tag_intro}} for more details.
#'
#' @usage @@title Topic title
setClass("TagTitle", contains = "Tag")
setMethod("writeRd", "TagTitle", function(tag) {
  new_command("title", tag@text)
})
