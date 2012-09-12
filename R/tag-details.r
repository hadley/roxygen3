#' Override default details.
#'
#' The topic title. By default this is taken from the third and subsequent 
#' paragraphs of the roxygen block. See \code{\link{tag_intro}} for more
#' details.
#'
#' @tagUsage @@details Text goes here.
setClass("TagDetails", contains = "Tag")
setMethod("writeRd", "TagDetails", function(object) {
  new_command("details", object@text)
})
