#' Automatically add the path as an Rd comment.
#'
#' This is useful for debugging Rd files, if you're not sure where the
#' components come from. It is not enabled by default - you need to use
#' \code{defaultBehaviour(debug = TRUE)} to turn it on.
#'
#' @tagUsage None: this is added automatically.
setClass("DebugTag", contains = "Tag",
  list(path = "character"))

setMethod("value", "DebugTag", function(tag) tag@path)

setMethod("defaultTag", c("DebugTag", "Object"), function(tag, object) {
  if (isNull(object)) return()
  new("DebugTag", path = location(object@srcref))
})

setMethod("writeRd", "DebugTag", function(object) {
  RdCommand("path", object@path)
})
