#' @usageTag @@exportClass class1 class2
#' @rdname tag-export
setClass("ExportClassTag", contains = "Tag")
setMethod("value<-", "ExportClassTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
setMethod("writeNamespace", "ExportClassTag", function(object) {
  ns_each("exportClass", object@text)
})
