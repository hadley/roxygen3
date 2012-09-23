#' @@seealso: other places to look for documentation.
#'
#' @tagUsage @@seealso Regular text, typically containing rd commands of the
#'  form \\code\{\\link\{mean\}\}.
setClass("SeealsoTag", contains = "Tag")
setMethod("writeRd", "SeealsoTag", function(object) {
  RdCommand("seealso", object@text)
})
