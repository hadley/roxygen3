#' Override default description.
#'
#' The topic title. By default this is taken from the second paragraph of the
#' roxygen block. See \code{\link{tag_intro}} for more details.
#'
#' @usageTag @@description Text goes here.
setClass("DescriptionTag", contains = "Tag")
setMethod("writeRd", "DescriptionTag", function(object) {
  RdCommand("description", object@text)
})
