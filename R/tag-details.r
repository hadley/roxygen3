#' Override default details.
#'
#' The topic title. By default this is taken from the third and subsequent
#' paragraphs of the roxygen block. See \code{\link{tag_intro}} for more
#' details.
#'
#' @usageTag @@details Text goes here.
setClass("DetailsTag", contains = "Tag")
setMethod("writeRd", "DetailsTag", function(object) {
  new_command("details", object@text)
})
