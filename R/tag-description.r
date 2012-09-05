#' Override default description.
#'
#' The topic title. By default this is taken from the second paragraph of the
#' roxygen block. See \code{\link{tag_intro}} for more details.
#'
#' @usage @@description Text goes here.
setClass("TagDescription", contains = "Tag")
setMethod("writeRd", "TagDescription", function(tag) {
  new_command("description", tag@text)
})
