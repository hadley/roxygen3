#' @usageTag @@exportPattern pattern
#' @rdname tag-export
setClass("ExportPatternTag", contains = "Tag")
setMethod("value<-", "ExportPatternTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
setMethod("writeNamespace", "ExportPatternTag", function(object) {
  ns_each("exportPattern", object@text)
})
