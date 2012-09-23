#' @@return: describe the output of the function
#'
#' @tagUsage @return What the function returns.  This can be a setnence of two
#'   or several paragraphs.
setClass("ReturnTag", contains = "Tag")

setMethod("writeRd", "ReturnTag", function(object) {
  RdCommand("value", object@text)
})
