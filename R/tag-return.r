#' @@return: describe the output of the function
#'
#' @tagUsage @return What the function returns.  This can be a sentence or two
#'   or several paragraphs. If the output is a list of several components,
#'   you can use \code{\item{comp_i}{Description of comp_i.}} to document them.
setClass("ReturnTag", contains = "Tag")

setMethod("writeRd", "ReturnTag", function(object) {
  RdCommand("value", object@text)
})
