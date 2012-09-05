#' Override default details.
#'
#' The topic title. By default this is taken from the third and subsequent 
#' paragraphs of the roxygen block. See \code{\link{tag_intro}} for more
#' details.
#'
#' @usage @@details Text goes here.
add_tag_roccer("details", text_tag())
setClass("TagDetails", contains = "Tag")
setMethod("writeRd", "TagDetails", function(tag) {
  new_command("details", tag@text)
})
