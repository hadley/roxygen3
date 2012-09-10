#' Add comments to the Rd file.
#'
#' @usage @@comment This is a comment that will not be user visible.

setClass("TagComment", contains = "Tag")

setMethod("writeRd", "TagComment", function(object) {
  new_command("comment", object@text)
})