#' @tagUsage @@exportMethods generic1 generic2
#' @rdname tag-export
setClass("ExportMethodsTag", contains = "Tag")
setMethod("value<-", "ExportMethodsTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})
setMethod("writeNamespace", "ExportMethodsTag", function(object) {
  ns_each("exportMethods", object@text)
})
