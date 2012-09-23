#' @@source: the source of a dataset
#'
#' @tagUsage @@source A description of the source of the dataset, which might
#'  include a \\url\{\} pointing to a source on the web.
setClass("SourceTag", contains = "Tag")
setMethod("writeRd", "SourceTag", function(object) {
  RdCommand("source", object@text)
})

