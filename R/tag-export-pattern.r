#' @tagUsage @@exportPattern pattern
#' @rdname tag-export
setClass("ExportPatternTag", contains = "Tag")
setMethod("value<-", "ExportPatternTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})
setMethod("writeNamespace", "ExportPatternTag", function(object) {
  ns_each("exportPattern", object@text)
})
