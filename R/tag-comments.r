#' @@comment: add comments to the Rd file.
#'
#' @tagUsage @@comment This is a comment that will not be user visible.
setClass("CommentTag", contains = "Tag")

setMethod("writeRd", "CommentTag", function(object) {
  RdCommand("comment", object@text)
})
