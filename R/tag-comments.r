#' Add comments to the Rd file.
#'
#' @usageTag @@comment This is a comment that will not be user visible.

setClass("CommentTag", contains = "Tag")

setMethod("writeRd", "CommentTag", function(object) {
  new_command("comment", object@text)
})
